use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
};

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse2, parse_quote,
    visit::Visit,
    GenericParam, Generics, Ident, ItemTrait, LifetimeParam, Result, TraitItem, TraitItemFn,
    TraitItemType, WhereClause, WherePredicate,
};

use proc_utils::PrettyPrint;

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

            macro_rules! __send_tokens {
                ($callback: path) => {
                    trait_impl_generics: #trait_impl_generics,
                    trait_use_generics: #trait_use_generics,
                    default_impl_generics: #default_impl_generics,
                    default_use_generics: #default_use_generics,
                    const_fns: { #(#const_fns)* },
                }
            }
        }
    };
    output.pretty_print();
    Ok(output)
}
