use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
};

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse2, parse_quote, GenericParam, Ident, ItemTrait, LifetimeParam, Result, TraitItem,
    TraitItemFn, TraitItemType,
};

use proc_utils::PrettyPrint;

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
    modified_trait.ident = parse_quote!(Trait);
    modified_trait.supertraits.push(parse_quote!(DefaultTypes));
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
    let mut seen_generics: HashSet<String> = HashSet::new();
    for trait_item_type in &defaults {
        for generic in &trait_item_type.generics.params {
            seen_generics.insert(generic.to_token_stream().to_string());
        }
        for bound in &trait_item_type.bounds {
            match bound {
                syn::TypeParamBound::Trait(_) => todo!(),
                syn::TypeParamBound::Lifetime(_) => todo!(),
                syn::TypeParamBound::Verbatim(_) => todo!(),
                _ => todo!(),
            }
        }
    }
    let output = quote! {
        #(#attrs)*
        pub mod #ident {
            use super::*;

            /// Contains default associated types for this SuperTrait
            pub struct Defaults;

            /// A subset of the original [`Trait`] containing just the default associated types
            /// _without_ their defaults. This is automatically implemented on [`Defaults`],
            /// which contains the actual default type values.
            pub trait DefaultTypes {
                #(#unfilled_defaults)*
            }

            impl DefaultTypes for Defaults {
                #(#defaults)*
            }

            #modified_trait
        }
    };
    output.pretty_print();
    Ok(output)
}

trait GetUsedGenerics {
    fn get_used_generics(&self) -> HashSet<String>;
}

impl GetUsedGenerics for GenericParam {
    fn get_used_generics(&self) -> HashSet<String> {
        match self {
            GenericParam::Lifetime(val) => val.get_used_generics(),
            GenericParam::Type(val) => todo!(),
            GenericParam::Const(val) => todo!(),
        }
    }
}

impl GetUsedGenerics for LifetimeParam {
    fn get_used_generics(&self) -> HashSet<String> {
        let mut lifetimes: HashSet<String> = HashSet::new();
        lifetimes.insert(self.lifetime.to_string());
        for bound in &self.bounds {
            lifetimes.insert(bound.to_string());
        }
        lifetimes
    }
}
