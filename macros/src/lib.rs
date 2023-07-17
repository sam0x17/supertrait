use macro_magic::import_tokens_attr;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_utils::PrettyPrint;
use quote::{quote, ToTokens};
use std::collections::HashSet;
use syn::{
    parse::{Nothing, Parse, ParseStream},
    parse2, parse_macro_input, parse_quote,
    spanned::Spanned,
    visit::Visit,
    Error, GenericParam, Generics, Ident, Item, ItemFn, ItemImpl, ItemMod, ItemTrait, Result,
    Signature, TraitItem, TraitItemFn, TraitItemType, WherePredicate,
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
    println!("subjects: {:?}", visitor.subjects);
    println!("usages: {:?}", visitor.usages);

    let default_generics = filter_generics(&modified_trait.generics, &visitor.usages);

    let default_impl_generics = default_generics.impl_generics;
    let default_use_generics = default_generics.use_generics;

    modified_trait.ident = parse_quote!(Trait);
    modified_trait
        .supertraits
        .push(parse_quote!(DefaultTypes #default_use_generics));

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
            }
        }
    };
    output.pretty_print();
    Ok(output)
}

#[doc(hidden)]
#[import_tokens_attr(::supertrait::__private::macro_magic)]
#[proc_macro_attribute]
pub fn __impl_supertrait(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    match impl_supertrait_internal(__custom_tokens, attr, tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

#[proc_macro_attribute]
pub fn impl_supertrait(attr: TokenStream, tokens: TokenStream) -> TokenStream {
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

/*
    mod exported_tokens {
        trait ConstFns {
            const fn something_else() -> usize;
        }
        fn trait_impl_generics() {}
        fn trait_use_generics() {}
        fn default_impl_generics() {}
        fn default_use_generics() {}
    }
*/

struct ExportedTokens {
    const_fns: Vec<TraitItemFn>,
    trait_impl_generics: Generics,
    trait_use_generics: Generics,
    default_impl_generics: Generics,
    default_use_generics: Generics,
}

impl TryFrom<ItemMod> for ExportedTokens {
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
        let Some(Item::Trait(ItemTrait { ident: const_fns_ident, items: const_fns, ..})) = main_body.get(0) else {
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

        let Some(Item::Fn(ItemFn { sig: Signature { ident: trait_impl_generics_ident, generics: trait_impl_generics, .. }, .. })) = main_body.get(1) else {
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

        let Some(Item::Fn(ItemFn { sig: Signature { ident: trait_use_generics_ident, generics: trait_use_generics, .. }, .. })) = main_body.get(2) else {
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

        let Some(Item::Fn(ItemFn { sig: Signature { ident: default_impl_generics_ident, generics: default_impl_generics, .. }, .. })) = main_body.get(3) else {
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

        let Some(Item::Fn(ItemFn { sig: Signature { ident: default_use_generics_ident, generics: default_use_generics, .. }, .. })) = main_body.get(4) else {
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

        Ok(ExportedTokens {
            const_fns,
            trait_impl_generics: trait_impl_generics.clone(),
            trait_use_generics: trait_use_generics.clone(),
            default_impl_generics: default_impl_generics.clone(),
            default_use_generics: default_use_generics.clone(),
        })
    }
}

fn impl_supertrait_internal(
    custom_tokens: impl Into<TokenStream2>,
    foreign_tokens: impl Into<TokenStream2>,
    item_tokens: impl Into<TokenStream2>,
) -> Result<TokenStream2> {
    let item_impl = parse2::<ItemImpl>(item_tokens.into())?;
    let foreign = ExportedTokens::try_from(parse2::<ItemMod>(foreign_tokens.into())?)?;
    let output = quote! {};
    Ok(output)
}
