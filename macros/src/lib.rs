use macro_magic::import_tokens_attr;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, TokenTree};
//use proc_utils::PrettyPrint;
use quote::{format_ident, quote, ToTokens};
use std::collections::{HashMap, HashSet};
use syn::{
    parse::{Nothing, Parse, ParseStream},
    parse2, parse_macro_input, parse_quote,
    spanned::Spanned,
    visit::Visit,
    Error, GenericParam, Generics, Ident, ImplItem, Item, ItemFn, ItemImpl, ItemMod, ItemTrait,
    Result, Signature, TraitItem, TraitItemFn, TraitItemType, WherePredicate,
};

mod generic_visitor;
use generic_visitor::*;

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
    let mut modified_trait = def.orig_trait;
    modified_trait.items = def.other_items;
    let ident = modified_trait.ident.clone();
    let attrs = modified_trait.attrs.clone();
    let defaults = def.types_with_defaults;
    let unfilled_defaults = defaults
        .iter()
        .cloned()
        .map(|mut typ| {
            typ.default = None;
            typ
        })
        .collect::<Vec<_>>();
    let mut visitor = FindGenericParam::new(&modified_trait.generics);
    for trait_item_type in &defaults {
        visitor.visit_trait_item_type(&trait_item_type)
    }
    // println!("subjects: {:?}", visitor.subjects);
    // println!("usages: {:?}", visitor.usages);

    let default_generics = filter_generics(&modified_trait.generics, &visitor.usages);

    let default_impl_generics = default_generics.impl_generics;
    let default_use_generics = default_generics.use_generics;

    modified_trait.ident = parse_quote!(Trait);
    // modified_trait
    //     .supertraits
    //     .push(parse_quote!(DefaultTypes #default_use_generics));

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
    trait_impl_generics_fn.sig.generics = trait_impl_generics;
    trait_use_generics_fn.sig.generics = trait_use_generics;
    default_impl_generics_fn.sig.generics = default_impl_generics.clone();
    default_use_generics_fn.sig.generics = default_use_generics.clone();

    modified_trait
        .items
        .extend(unfilled_defaults.iter().map(|item| parse_quote!(#item)));

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
                #(#unfilled_defaults)*
            }

            impl #default_impl_generics DefaultTypes #default_use_generics for Defaults {
                #(#defaults)*
            }

            #modified_trait

            #[::supertrait::__private::macro_magic::export_tokens_no_emit]
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
    // output.pretty_print();
    Ok(output)
}

#[doc(hidden)]
#[import_tokens_attr(::supertrait::__private::macro_magic)]
#[allow(non_snake_case)]
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
    };
    quote! {
        #[::supertrait::__impl_supertrait(#trait_being_impled::exported_tokens)]
        #item_impl
    }
    .into()
}

struct ImportedTokens {
    const_fns: Vec<TraitItemFn>,
    trait_impl_generics: Generics,
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

impl<T: ToTokens> ForceGetIdent for T {}

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
        trait_use_generics,
        default_impl_generics: _,
        default_use_generics,
        default_items,
    } = ImportedTokens::try_from(parse2::<ItemMod>(foreign_tokens.into())?)?;

    let trait_mod = trait_path.clone();
    *trait_path = parse_quote!(#trait_mod::Trait #trait_use_generics);

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

    let mut impl_const_fn_sigs: HashSet<String> = HashSet::new();
    let mut impl_const_fns: Vec<ImplItem>;
    (impl_const_fns, item_impl.items) = item_impl.items.into_iter().partition(|item| {
        let ImplItem::Fn(impl_item_fn) = item else { return false };
        if impl_item_fn.sig.constness.is_none() {
            return false;
        };
        impl_const_fn_sigs.insert(impl_item_fn.sig.to_token_stream().to_string());
        true
    });
    for const_fn in impl_const_fns.iter_mut() {
        let ImplItem::Fn(const_fn) = const_fn else { unreachable!() };
        const_fn.vis = parse_quote!(pub);
    }
    for item in &const_fns {
        if !impl_const_fn_sigs.contains(&item.sig.to_token_stream().to_string()) {
            return Err(Error::new(
                item_impl.span(),
                format!("missing impl for `{}`.", item.sig.ident),
            ));
        }
    }

    let trait_import_name: Ident = format_ident!("{}Trait", item_impl.force_get_ident());

    let output = quote! {

        #item_impl

        impl #trait_impl_generics #impl_target {
            #(#impl_const_fns)*
        }

        #[allow(unused)]
        use #trait_mod::Trait as #trait_import_name;
    };
    // println!("impl:");
    // output.pretty_print();
    Ok(output)
}
