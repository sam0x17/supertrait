#![allow(non_snake_case)]

use macro_magic::import_tokens_attr;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, TokenTree};
use quote::{format_ident, quote, ToTokens};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    sync::atomic::AtomicU64,
};
use syn::{
    parse::{Nothing, Parse, ParseStream},
    parse2, parse_macro_input, parse_quote, parse_str,
    spanned::Spanned,
    visit::Visit,
    visit_mut::VisitMut,
    Error, GenericParam, Generics, Ident, ImplItem, ImplItemFn, Item, ItemFn, ItemImpl, ItemMod,
    ItemTrait, Path, Result, Signature, TraitItem, TraitItemFn, TraitItemType, Visibility,
    WherePredicate,
};

#[cfg(feature = "debug")]
use proc_utils::PrettyPrint;

mod generic_visitor;
use generic_visitor::*;

static IMPL_COUNT: AtomicU64 = AtomicU64::new(0);
thread_local! {
    static SUPERTRAIT_PATH: RefCell<String> = RefCell::new(String::from("::supertrait"));
}

fn get_supertrait_path() -> Path {
    SUPERTRAIT_PATH.with(|p| parse_str(p.borrow().clone().as_str()).unwrap())
}

#[proc_macro]
pub fn set_supertrait_path(tokens: TokenStream) -> TokenStream {
    let path = parse_macro_input!(tokens as Path);
    SUPERTRAIT_PATH.with(|p| p.replace(path.to_token_stream().to_string()));
    quote!().into()
}

struct SuperTraitDef {
    pub orig_trait: ItemTrait,
    pub const_fns: Vec<TraitItemFn>,
    pub types_with_defaults: Vec<TraitItemType>,
    pub other_items: Vec<TraitItem>,
}

impl Parse for SuperTraitDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let orig_trait = input.parse::<ItemTrait>()?;
        let mut const_fns: Vec<TraitItemFn> = Vec::new();
        let mut types_with_defaults: Vec<TraitItemType> = Vec::new();
        let mut other_items: Vec<TraitItem> = Vec::new();
        for trait_item in &orig_trait.items {
            match trait_item {
                TraitItem::Fn(trait_item_fn) => match trait_item_fn.sig.constness {
                    Some(_) => const_fns.push(trait_item_fn.clone()),
                    None => other_items.push(trait_item.clone()),
                },
                TraitItem::Type(typ) => match typ.default {
                    Some(_) => types_with_defaults.push(typ.clone()),
                    None => other_items.push(trait_item.clone()),
                },
                other_item => other_items.push(other_item.clone()),
            }
        }
        Ok(SuperTraitDef {
            orig_trait,
            const_fns,
            types_with_defaults,
            other_items,
        })
    }
}

struct FilteredGenerics {
    /// Represents the `#` in `impl<*> Something for MyStruct<#>`. Both variants share the same
    /// where clause
    use_generics: Generics,
    /// Represents the `*` in `impl<*> Something for MyStruct<#>`. Both variants share the same
    /// where clause
    impl_generics: Generics,
}

fn filter_generics(generics: &Generics, whitelist: &HashSet<GenericUsage>) -> FilteredGenerics {
    let filtered_generic_params = generics
        .params
        .iter()
        .cloned()
        .filter(|g| whitelist.contains(&g.into()));

    // generate a version of the where clause containing only whitelisted usages
    let filtered_where_clause = match &generics.where_clause {
        Some(where_clause) => {
            let mut where_clause = where_clause.clone();
            let predicates_filtered = where_clause.predicates.iter().filter(|p| match *p {
                WherePredicate::Lifetime(lifetime) => {
                    whitelist.contains(&GenericUsage::from_lifetime(&lifetime.lifetime))
                }
                WherePredicate::Type(typ) => {
                    whitelist.contains(&GenericUsage::from_type(&typ.bounded_ty))
                }
                _ => unimplemented!(),
            });
            where_clause.predicates = parse_quote!(#(#predicates_filtered),*);
            Some(where_clause)
        }
        None => None,
    };

    let use_generic_params = filtered_generic_params.clone().map(|g| match g {
        GenericParam::Lifetime(lifetime) => lifetime.lifetime.to_token_stream(),
        GenericParam::Type(typ) => typ.ident.to_token_stream(),
        GenericParam::Const(constant) => constant.ident.to_token_stream(),
    });

    let use_generics = Generics {
        lt_token: parse_quote!(<),
        params: parse_quote!(#(#use_generic_params),*),
        gt_token: parse_quote!(>),
        where_clause: filtered_where_clause.clone(),
    };

    let impl_generics = Generics {
        lt_token: parse_quote!(<),
        params: parse_quote!(#(#filtered_generic_params),*),
        gt_token: parse_quote!(>),
        where_clause: filtered_where_clause,
    };

    FilteredGenerics {
        use_generics,
        impl_generics,
    }
}

#[proc_macro_attribute]
pub fn supertrait(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    match supertrait_internal(attr, tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

fn supertrait_internal(
    attr: impl Into<TokenStream2>,
    tokens: impl Into<TokenStream2>,
) -> Result<TokenStream2> {
    parse2::<Nothing>(attr.into())?;
    let def = parse2::<SuperTraitDef>(tokens.into())?;
    let export_tokens_ident = format_ident!("{}_exported_tokens", def.orig_trait.ident);
    let mut modified_trait = def.orig_trait;
    modified_trait.items = def.other_items;
    let ident = modified_trait.ident.clone();
    let attrs = modified_trait.attrs.clone();
    let mut defaults = def.types_with_defaults;
    let unfilled_defaults = defaults
        .iter()
        .cloned()
        .map(|mut typ| {
            typ.default = None;
            typ
        })
        .collect::<Vec<_>>();
    let mut visitor = FindGenericParam::new(&modified_trait.generics);
    let mut replace_self = ReplaceSelfType {
        replace_type: parse_quote!(__Self),
    };
    for trait_item_type in &mut defaults {
        visitor.visit_trait_item_type(trait_item_type);
        replace_self.visit_trait_item_type_mut(trait_item_type);
    }

    let mut default_generics = filter_generics(&modified_trait.generics, &visitor.usages);
    default_generics
        .impl_generics
        .params
        .push(parse_quote!(__Self));
    default_generics
        .use_generics
        .params
        .push(parse_quote!(__Self));

    let default_impl_generics = default_generics.impl_generics;
    let default_use_generics = default_generics.use_generics;

    modified_trait.ident = parse_quote!(Trait);

    let trait_use_generic_params = modified_trait.generics.params.iter().map(|g| match g {
        GenericParam::Lifetime(lifetime) => lifetime.lifetime.to_token_stream(),
        GenericParam::Type(typ) => typ.ident.to_token_stream(),
        GenericParam::Const(constant) => constant.ident.to_token_stream(),
    });

    let trait_impl_generics = modified_trait.generics.clone();
    let trait_use_generics = Generics {
        lt_token: parse_quote!(<),
        params: parse_quote!(#(#trait_use_generic_params),*),
        gt_token: parse_quote!(>),
        where_clause: modified_trait.generics.where_clause.clone(),
    };

    let const_fns = def.const_fns;
    let mut trait_impl_generics_fn: ItemFn = parse_quote! { fn trait_impl_generics() {} };
    let mut trait_use_generics_fn: ItemFn = parse_quote! { fn trait_use_generics() {} };
    let mut default_impl_generics_fn: ItemFn = parse_quote! { fn default_impl_generics() {} };
    let mut default_use_generics_fn: ItemFn = parse_quote! { fn default_use_generics() {} };
    trait_impl_generics_fn.sig.generics = trait_impl_generics.clone();
    trait_use_generics_fn.sig.generics = trait_use_generics;
    default_impl_generics_fn.sig.generics = default_impl_generics.clone();
    default_use_generics_fn.sig.generics = default_use_generics.clone();

    modified_trait
        .items
        .extend(unfilled_defaults.iter().map(|item| parse_quote!(#item)));

    let converted_const_fns = const_fns.iter().map(|const_fn| {
        let mut item = const_fn.clone();
        item.sig.constness = None;
        let item: TraitItem = parse_quote!(#item);
        item
    });
    modified_trait.items.extend(converted_const_fns);

    let supertrait_path = get_supertrait_path();

    let output = quote! {
        #(#attrs)*
        #[allow(non_snake_case)]
        pub mod #ident {
            use super::*;

            /// Contains default associated types for this SuperTrait
            pub struct Defaults;

            /// A subset of the original [`Trait`] containing just the default associated types
            /// _without_ their defaults. This is automatically implemented on [`Defaults`],
            /// which contains the actual default type values.
            pub trait DefaultTypes #default_impl_generics {
                type __Self;
                #(#unfilled_defaults)*
            }

            impl #default_impl_generics DefaultTypes #default_use_generics for Defaults {
                #(#defaults)*
                type __Self = ();
            }

            #modified_trait

            #[#supertrait_path::__private::macro_magic::export_tokens_no_emit(#export_tokens_ident)]
            mod exported_tokens {
                trait ConstFns {
                    #(#const_fns)*
                }

                #trait_impl_generics_fn
                #trait_use_generics_fn
                #default_impl_generics_fn
                #default_use_generics_fn

                mod default_items {
                    #(#defaults)*
                }
            }
        }
    };
    #[cfg(feature = "debug")]
    output.pretty_print();
    Ok(output)
}

#[doc(hidden)]
#[import_tokens_attr(format!(
    "{}::__private::macro_magic",
    get_supertrait_path().to_token_stream().to_string()
))]
#[proc_macro_attribute]
pub fn __impl_supertrait(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    match impl_supertrait_internal(attr, tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

#[proc_macro_attribute]
pub fn impl_supertrait(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    parse_macro_input!(attr as Nothing);
    let item_impl = parse_macro_input!(tokens as ItemImpl);
    let trait_being_impled = match item_impl.trait_.clone() {
        Some((_, path, _)) => path,
        None => return Error::new(
            item_impl.span(),
            "Supertrait impls must have a trait being implemented. Inherent impls are not supported."
        ).into_compile_error().into(),
    }.strip_trailing_generics();
    let supertrait_path = get_supertrait_path();
    let export_tokens_ident = format_ident!(
        "{}_exported_tokens",
        trait_being_impled.segments.last().unwrap().ident
    );
    let output = quote! {
        #[#supertrait_path::__impl_supertrait(#trait_being_impled::#export_tokens_ident)]
        #item_impl
    };
    // #[cfg(feature = "debug")]
    // output.pretty_print();
    output.into()
}

struct ImportedTokens {
    const_fns: Vec<TraitItemFn>,
    trait_impl_generics: Generics,
    #[allow(unused)]
    trait_use_generics: Generics,
    #[allow(unused)]
    default_impl_generics: Generics,
    default_use_generics: Generics,
    default_items: Vec<TraitItem>,
}

impl TryFrom<ItemMod> for ImportedTokens {
    type Error = Error;

    fn try_from(item_mod: ItemMod) -> std::result::Result<Self, Self::Error> {
        if item_mod.ident != "exported_tokens" {
            return Err(Error::new(
                item_mod.ident.span(),
                "expected `exported_tokens`.",
            ));
        }
        let item_mod_span = item_mod.span();
        let Some((_, main_body)) = item_mod.content else {
            return Err(Error::new(
                item_mod_span,
                "`exported_tokens` module must have a defined body."
            ));
        };
        let Some(Item::Trait(ItemTrait {
            ident: const_fns_ident,
            items: const_fns,
        ..})) = main_body.get(0) else {
            return Err(Error::new(
                item_mod_span,
                "the first item in `exported_tokens` should be a trait called `ConstFns`.",
            ));
        };
        if const_fns_ident != "ConstFns" {
            return Err(Error::new(const_fns_ident.span(), "expected `ConstFns`."));
        }

        let const_fns: Vec<TraitItemFn> = const_fns
            .into_iter()
            .map(|item| match item {
                TraitItem::Fn(trait_item_fn) => Ok(trait_item_fn.clone()),
                _ => return Err(Error::new(item.span(), "expected `fn`")), // can't do this
            })
            .collect::<std::result::Result<_, Self::Error>>()?;

        let Some(Item::Fn(ItemFn { sig: Signature {
            ident: trait_impl_generics_ident,
            generics: trait_impl_generics,
            ..
        }, .. })) = main_body.get(1) else {
            return Err(Error::new(
                item_mod_span,
                "the second item in `exported_tokens` should be an fn called `trait_impl_generics`.",
            ));
        };
        if trait_impl_generics_ident != "trait_impl_generics" {
            return Err(Error::new(
                trait_impl_generics_ident.span(),
                "expected `trait_impl_generics`.",
            ));
        }

        let Some(Item::Fn(ItemFn { sig: Signature {
            ident: trait_use_generics_ident,
            generics: trait_use_generics,
            ..
        }, .. })) = main_body.get(2) else {
            return Err(Error::new(
                item_mod_span,
                "the third item in `exported_tokens` should be an fn called `trait_use_generics`.",
            ));
        };
        if trait_use_generics_ident != "trait_use_generics" {
            return Err(Error::new(
                trait_use_generics_ident.span(),
                "expected `trait_use_generics`.",
            ));
        }

        let Some(Item::Fn(ItemFn { sig: Signature {
            ident: default_impl_generics_ident,
            generics: default_impl_generics,
            ..
        }, .. })) = main_body.get(3) else {
            return Err(Error::new(
                item_mod_span,
                "the fourth item in `exported_tokens` should be an fn called `default_impl_generics`.",
            ));
        };
        if default_impl_generics_ident != "default_impl_generics" {
            return Err(Error::new(
                default_impl_generics_ident.span(),
                "expected `default_impl_generics`.",
            ));
        }

        let Some(Item::Fn(ItemFn { sig: Signature {
            ident: default_use_generics_ident,
            generics: default_use_generics,
            ..
        }, .. })) = main_body.get(4) else {
            return Err(Error::new(
                item_mod_span,
                "the fifth item in `exported_tokens` should be an fn called `default_use_generics`.",
            ));
        };
        if default_use_generics_ident != "default_use_generics" {
            return Err(Error::new(
                default_use_generics_ident.span(),
                "expected `default_use_generics`.",
            ));
        }

        let Some(Item::Mod(default_items_mod)) = main_body.get(5) else {
            return Err(Error::new(
                item_mod_span,
                "the sixth item in `exported_tokens` should be a module called `default_items_mod`.",
            ));
        };
        if default_items_mod.ident != "default_items" {
            return Err(Error::new(
                default_items_mod.ident.span(),
                "expected `default_items`.",
            ));
        }
        let Some((_, default_items)) = default_items_mod.content.clone() else {
            return Err(Error::new(
                default_items_mod.ident.span(),
                "`default_items` item must be an inline module.",
            ));
        };
        let default_items: Vec<TraitItem> = default_items
            .iter()
            .map(|item| parse_quote!(#item))
            .collect();

        Ok(ImportedTokens {
            const_fns,
            trait_impl_generics: trait_impl_generics.clone(),
            trait_use_generics: trait_use_generics.clone(),
            default_impl_generics: default_impl_generics.clone(),
            default_use_generics: default_use_generics.clone(),
            default_items: default_items,
        })
    }
}

trait GetIdent {
    fn get_ident(&self) -> Option<Ident>;
}

impl GetIdent for TraitItem {
    fn get_ident(&self) -> Option<Ident> {
        use TraitItem::*;
        match self {
            Const(item_const) => Some(item_const.ident.clone()),
            Fn(item_fn) => Some(item_fn.sig.ident.clone()),
            Type(item_type) => Some(item_type.ident.clone()),
            _ => None,
        }
    }
}

impl GetIdent for ImplItem {
    fn get_ident(&self) -> Option<Ident> {
        use ImplItem::*;
        match self {
            Const(item_const) => Some(item_const.ident.clone()),
            Fn(item_fn) => Some(item_fn.sig.ident.clone()),
            Type(item_type) => Some(item_type.ident.clone()),
            _ => None,
        }
    }
}

trait FlattenGroups {
    fn flatten_groups(&self) -> TokenStream2;
}

impl FlattenGroups for TokenStream2 {
    fn flatten_groups(&self) -> TokenStream2 {
        let mut iter = self.clone().into_iter();
        let mut final_tokens = TokenStream2::new();
        while let Some(token) = iter.next() {
            if let TokenTree::Group(group) = &token {
                let flattened = group.stream().flatten_groups();
                final_tokens.extend(quote!(<#flattened>));
                continue;
            }
            final_tokens.extend([token]);
        }
        final_tokens
    }
}

trait ForceGetIdent: ToTokens {
    fn force_get_ident(&self) -> Ident {
        let mut iter = self.to_token_stream().flatten_groups().into_iter();
        let mut final_tokens = TokenStream2::new();
        while let Some(token) = iter.next() {
            let mut tmp = final_tokens.clone();
            tmp.extend([token.clone()]);
            if parse2::<Ident>(tmp).is_ok() {
                final_tokens.extend([token]);
            }
        }
        parse_quote!(#final_tokens)
    }
}

trait StripTrailingGenerics {
    fn strip_trailing_generics(&self) -> Self;
}

impl StripTrailingGenerics for Path {
    fn strip_trailing_generics(&self) -> Self {
        let mut tmp = self.clone();
        let Some(last) = tmp.segments.last_mut() else { unreachable!() };
        let ident = last.ident.clone();
        *last = parse_quote!(#ident);
        tmp
    }
}

impl<T: ToTokens> ForceGetIdent for T {}

fn merge_generics(a: &Generics, b: &Generics) -> Generics {
    let mut params = b.params.clone();
    params.extend(a.params.clone());
    let where_clause = match &a.where_clause {
        Some(a_where) => match &b.where_clause {
            Some(b_where) => {
                let mut combined_where = b_where.clone();
                combined_where.predicates.extend(a_where.predicates.clone());
                Some(combined_where)
            }
            None => a.where_clause.clone(),
        },
        None => b.where_clause.clone(),
    };
    Generics {
        lt_token: b.lt_token.clone(),
        params,
        gt_token: b.gt_token.clone(),
        where_clause: where_clause,
    }
}

struct ReplaceType {
    search_type: RemappedGeneric,
    replace_type: GenericParam,
}

impl VisitMut for ReplaceType {
    fn visit_ident_mut(&mut self, ident: &mut Ident) {
        let search_ident = self.search_type.ident();
        if ident != search_ident {
            return;
        }
        *ident = self.replace_type.force_get_ident();
    }
}

struct ReplaceSelfType {
    replace_type: Ident,
}

impl VisitMut for ReplaceSelfType {
    fn visit_ident_mut(&mut self, ident: &mut Ident) {
        if ident != "Self" {
            return;
        }
        *ident = self.replace_type.clone();
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
enum RemappedGeneric {
    Lifetime(Ident),
    Type(Ident),
    Const(Ident),
}

impl RemappedGeneric {
    fn ident(&self) -> &Ident {
        match self {
            RemappedGeneric::Lifetime(ident) => ident,
            RemappedGeneric::Type(ident) => ident,
            RemappedGeneric::Const(ident) => ident,
        }
    }
}

fn impl_supertrait_internal(
    foreign_tokens: impl Into<TokenStream2>,
    item_tokens: impl Into<TokenStream2>,
) -> Result<TokenStream2> {
    let mut item_impl = parse2::<ItemImpl>(item_tokens.into())?;
    let Some((_, trait_path, _)) = &mut item_impl.trait_ else {
        return Err(Error::new(
            item_impl.span(),
            "#[impl_supertrait] can only be attached to non-inherent impls involving a trait \
            that has `#[supertrait]` attached to it."
        ));
    };
    let impl_target = item_impl.self_ty.clone();

    let ImportedTokens {
        const_fns,
        trait_impl_generics,
        trait_use_generics: _,
        default_impl_generics: _,
        default_use_generics,
        default_items,
    } = ImportedTokens::try_from(parse2::<ItemMod>(foreign_tokens.into())?)?;

    let mut remapped_type_params: HashMap<RemappedGeneric, GenericParam> = HashMap::new();
    for (i, param) in trait_impl_generics.params.iter().enumerate() {
        let remapped = match param {
            GenericParam::Lifetime(lifetime) => {
                RemappedGeneric::Lifetime(lifetime.lifetime.ident.clone())
            }
            GenericParam::Type(typ) => RemappedGeneric::Type(typ.ident.clone()),
            GenericParam::Const(constant) => RemappedGeneric::Const(constant.ident.clone()),
        };
        let last_seg = trait_path.segments.last().unwrap();
        let args: Vec<TokenStream2> = match last_seg.arguments.clone() {
            syn::PathArguments::None => continue,
            syn::PathArguments::AngleBracketed(args) => {
                args.args.into_iter().map(|a| a.to_token_stream()).collect()
            }
            syn::PathArguments::Parenthesized(args) => args
                .inputs
                .into_iter()
                .map(|a| a.to_token_stream())
                .collect(),
        };

        if i >= args.len() {
            continue;
        }
        let target = args[i].clone();
        let target: GenericParam = parse_quote!(#target);
        remapped_type_params.insert(remapped, target);
    }

    // replace Self type with type we are implementing on
    remapped_type_params.insert(
        RemappedGeneric::Type(parse_quote!(__Self)),
        parse_quote!(#impl_target),
    );

    let trait_mod = trait_path.clone().strip_trailing_generics();
    trait_path
        .segments
        .insert(trait_path.segments.len() - 1, parse_quote!(#trait_mod));
    trait_path.segments.last_mut().unwrap().ident = parse_quote!(Trait);

    let mut final_items: HashMap<Ident, ImplItem> = HashMap::new();
    for item in default_items {
        let item_ident = item.get_ident().unwrap();
        let mut item: ImplItem = parse_quote!(#item);
        use ImplItem::*;
        match &mut item {
            Const(item_const) => {
                item_const.expr = parse_quote!(<#trait_mod::Defaults as #trait_mod::DefaultTypes #default_use_generics>::#item_ident)
            }
            Type(item_type) => {
                item_type.ty = parse_quote!(<#trait_mod::Defaults as #trait_mod::DefaultTypes #default_use_generics>::#item_ident)
            }
            _ => unimplemented!("this item has no notion of defaults"),
        }
        for search in remapped_type_params.keys() {
            let replace = &remapped_type_params[search];
            let mut visitor = ReplaceType {
                search_type: search.clone(),
                replace_type: replace.clone(),
            };
            visitor.visit_impl_item_mut(&mut item);
        }
        if item_ident == "__Self" {
            item = parse_quote!(#impl_target);
        }
        final_items.insert(item_ident, item);
    }
    let mut final_verbatim_items: Vec<ImplItem> = Vec::new();
    for item in &item_impl.items {
        let Some(item_ident) = item.get_ident() else {
            final_verbatim_items.push(item.clone());
            continue;
        };
        final_items.insert(item_ident, item.clone());
    }

    let mut final_items = final_items.values().cloned().collect::<Vec<_>>();
    final_items.extend(final_verbatim_items);
    item_impl.items = final_items;

    let mut impl_const_fn_idents: HashSet<Ident> = HashSet::new();
    let mut impl_const_fns: Vec<ImplItem>;
    (impl_const_fns, item_impl.items) = item_impl.items.into_iter().partition(|item| {
        let ImplItem::Fn(impl_item_fn) = item else { return false };
        if impl_item_fn.sig.constness.is_none() {
            return false;
        };
        impl_const_fn_idents.insert(impl_item_fn.sig.ident.clone());
        true
    });

    //
    for item in &const_fns {
        if !impl_const_fn_idents.contains(&item.sig.ident) {
            if item.default.is_none() {
                return Err(Error::new(
                    item_impl.span(),
                    format!("missing impl for `{}`.", item.sig.ident),
                ));
            }
            impl_const_fns.push(parse_quote!(#item));
        }
    }
    for const_fn in impl_const_fns.iter_mut() {
        let ImplItem::Fn(const_fn) = const_fn else { unreachable!() };
        const_fn.vis = parse_quote!(pub);
    }

    let impl_index = IMPL_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    let trait_import_name: Ident = format_ident!(
        "{}{}TraitImpl_{}",
        impl_target.clone().force_get_ident(),
        item_impl.trait_.clone().unwrap().1.force_get_ident(),
        impl_index,
    );

    let converted_const_fns = impl_const_fns.iter().map(|const_fn| {
        let mut const_fn: ImplItemFn = parse_quote!(#const_fn);
        const_fn.sig.constness = None;
        const_fn.vis = Visibility::Inherited;
        let item: ImplItem = parse_quote!(#const_fn);
        item
    });

    let impl_const_fns = impl_const_fns.iter().map(|const_fn| {
        let mut const_fn_visitor = FindGenericParam::new(&trait_impl_generics);
        const_fn_visitor.visit_impl_item(const_fn);
        let const_fn_generics =
            filter_generics(&trait_impl_generics, &const_fn_visitor.usages).impl_generics;
        let mut const_fn: ImplItemFn = parse_quote!(#const_fn);
        const_fn.sig.generics = merge_generics(&const_fn_generics, &const_fn.sig.generics);
        const_fn
    });

    item_impl.items.extend(converted_const_fns);
    let mut impl_visitor = FindGenericParam::new(&item_impl.generics);
    impl_visitor.visit_item_impl(&item_impl);
    item_impl.generics = filter_generics(&item_impl.generics, &impl_visitor.usages).impl_generics;

    let inherent_impl = if impl_const_fns.len() > 0 {
        Some(quote! {
            // const fn implementations
            impl #impl_target {
                #(#impl_const_fns)*
            }
        })
    } else {
        None
    };

    let output = quote! {
        #item_impl

        #inherent_impl

        #[allow(unused)]
        use #trait_mod::Trait as #trait_import_name;
    };
    #[cfg(feature = "debug")]
    output.pretty_print();
    Ok(output)
}
