use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse2, ItemFn, ItemTrait, Result, TraitItem, TraitItemFn, TraitItemType,
};

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
    assert_eq!(def.const_fns.len(), 1);
    assert_eq!(def.types_with_defaults.len(), 1);
    Ok(quote!())
}
