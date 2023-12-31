#![allow(non_snake_case)]
#![warn(missing_docs)]

//! Contains support macros for [supertrait](https://crates.io/crates/supertrait).

use macro_magic::import_tokens_attr;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, TokenTree};
use quote::{format_ident, quote, ToTokens};
use rand::{distributions::Standard, prelude::Distribution, Rng};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::atomic::AtomicU64,
};
use syn::{
    parse::{Nothing, Parse, ParseStream},
    parse2, parse_macro_input, parse_quote, parse_str,
    punctuated::Punctuated,
    spanned::Spanned,
    visit::Visit,
    visit_mut::VisitMut,
    Error, GenericParam, Generics, Ident, ImplItem, ImplItemFn, Item, ItemFn, ItemImpl, ItemMod,
    ItemTrait, Path, Result, Signature, TraitItem, TraitItemFn, TraitItemType, TypePath,
    Visibility, WherePredicate,
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

fn random<T>() -> T
where
    Standard: Distribution<T>,
{
    let mut rng = rand::thread_rng();
    rng.gen()
}

/// Allows you to override the module path where supertrait's macros will look for necessary
/// re-exports (such as `macro_magic`).
///
/// The default is `::supertrait`.
///
/// Generally speaking you shouldn't need to use this directly, but in some scenarios (like in
/// the `lib.rs` of the main crate, this is necessary).
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
    has_defaults: HashSet<Ident>,
}

impl FilteredGenerics {
    fn strip_default_generics(&mut self) {
        // find generic params in impl_generics that have defaults
        let has_defaults = self
            .impl_generics
            .params
            .iter()
            .filter_map(|g| match g {
                GenericParam::Lifetime(_) => None,
                GenericParam::Type(typ) => match typ.default {
                    Some(_) => Some(typ.force_get_ident()),
                    None => None,
                },
                GenericParam::Const(constant) => match constant.default {
                    Some(_) => Some(constant.force_get_ident()), // might be wrong
                    None => None,
                },
            })
            .collect::<HashSet<Ident>>();
        // strip these from impl_generics
        self.impl_generics.params = self
            .impl_generics
            .params
            .iter()
            .filter(|g| !has_defaults.contains(&g.force_get_ident()))
            .cloned()
            .collect();
        // also strip them from use_generics
        self.use_generics.params = self
            .use_generics
            .params
            .iter()
            .filter(|g| !has_defaults.contains(&g.force_get_ident()))
            .cloned()
            .collect();
        self.has_defaults = has_defaults;
    }
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
        has_defaults: HashSet::new(),
    }
}

struct DefaultRemover;

impl VisitMut for DefaultRemover {
    fn visit_generics_mut(&mut self, generics: &mut Generics) {
        for param in &mut generics.params {
            if let syn::GenericParam::Type(ref mut type_param) = param {
                type_param.default = None;
            }
        }
    }
}

trait PunctuatedExtension<T: ToTokens + Clone, P: ToTokens + std::default::Default>: Sized {
    fn push_front(&mut self, value: T);
}

impl<T: ToTokens + Clone, P: ToTokens + std::default::Default> PunctuatedExtension<T, P>
    for Punctuated<T, P>
{
    fn push_front(&mut self, value: T) {
        let mut new_punctuated = Punctuated::new();
        new_punctuated.push(value);
        new_punctuated.extend(self.iter().cloned());
        *self = new_punctuated;
    }
}

/// Attach this attribute to a trait definition to transform it into a supertrait, able to make
/// use of _default associated types_ and const fn trait items (the latter with some
/// limitations).
///
/// The following example demonstrates some of the major features and edge cases of supertraits:
///
/// ```ignore
/// #[supertrait]
/// pub trait Fizz<T: Copy>: Copy + Sized {
///     type Foo = Option<T>;
///     type Bar;
///
///     const fn double_value(val: T) -> (T, T) {
///         (val, val)
///     }
///
///     const fn triple_value(val: T) -> (T, T, T);
///
///     fn double_self_plus(&self, plus: Self::Foo) -> (Self, Self, Self::Foo) {
///         (*self, *self, plus)
///     }
///
///     const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T);
/// }
/// ```
///
/// ### Default Associated Types
///
/// Supertrait supports default associated types in a supertrait definition. These associated
/// types can also be used in other trait items via `Self::SomeType` and this will work
/// properly anywhere in the trait or trait impl.
///
/// Implementers are free to override default types specified on the trait by simply impling
/// that trait item.
///
/// In practice, default associated types in supertrait behave exactly the way you would expect
/// them to behave when they finally reach stable Rust.
///
/// Generics are fully supported by this feature, and any types you mention in your default
/// associated type will automatically be in scope when you
/// [`#[impl_supertrait]`](`macro@impl_supertrait`) the trait.
///
///
/// ### Const Fn Trait Items
/// Supertrait also supports const fn trait items. These items are masked by auto-generated
/// non-const versions of the const fns that are added to enforce trait bounds.
///
/// Currently there is no way in stable Rust to add a const fn to a trait. To accomplish
/// something like this, supertrait does two things:
///
/// - First, in the expansion of [`#[impl_supertrait]`](`macro@impl_supertrait`), the const fns
///   are automatically implemented as _inherents_ on the implementing type. This creates major
///   limitations and makes it impossible to do things like blanket impls if they involve const
///   fns.
/// - Second, non-const copies that mask each const fn trait item are created and injected into
///   the resulting supertrait. This allow us to ensure all trait bounds are respected by
///   implementers.
///
/// Thus all bounds and trait requirements are enforced on the implementing type, however const
/// fns in particular are implemented as inherents, i.e. `impl MyStruct { const fn something()
/// {...} }`.
///
/// This technique has a few limitations. Because of naming collisions on inherent impls, you
/// can't impl the same (const-fn-containing) supertrait on the same type multiple times with
/// different generics, like you can with famous conventional traits like `From<T>`.
///
/// For more information, see the docs for [`#[impl_supertrait]`](`macro@impl_supertrait`).
///
/// ### Expansion
///
/// Supertrait relies heavily on the token teleportation capabilities provided by
/// [macro_magic](https://crates.io/crates/macro_magic). As a result, special care has to be
/// taken to ensure any in-scope types mentioned in the teleported areas of a supertrait are
/// accessible anywhere the supertrait is implemented.
///
/// To accomplish this, supertrait uses the "module wormhole" technique, whereby the actual
/// trait item (i.e. `MyTrait`) is represented as a module containing `use super::*;`, which in turn "teleports" all local
/// imports from the trait definition site to any context in which the trait is implemented.
/// Under the hood, the actual generated trait lives in `MyTrait::Trait`, along with a trait
/// and struct pair called `DefaultTypes` and `Defaults` (the latter, containing an impl
/// specifying all of the default types with their proper generics). That said, inside impls
/// for a supertrait called `MyTrait`, you can refer to the trait like `MyTrait` instead of
/// `MyTrait::Trait` as well as referring to the associated types like `Self::Whatever`
/// directly.
///
/// The following is the intermediate expansion for the `#[supertrait]` trait definition shown
/// above, decorated with comments explaining what the various parts do:
///
/// ```ignore
/// #[allow(non_snake_case)]
/// pub mod Fizz {
///     // "wormhole technique", this allows us to capture the trait definition import scope
///     // and re-use it at any trait impl sites.
///     use super::*;
///
///     /// Contains default associated types for this SuperTrait
///     pub struct Defaults;
///
///     /// A subset of the original [`Trait`] containing just the default associated types
///     /// _without_ their defaults. This is automatically implemented on [`Defaults`],
///     /// which contains the actual default type values.
///     pub trait DefaultTypes<T: Copy, __Self> {
///         type __Self;
///         type Foo;
///     }
///
///     // note that the `__Self` keyword is used internally in place of `Self`, which is
///     // replaced back with `Self` at the trait impl site
///     impl<T: Copy, __Self> DefaultTypes<T, __Self> for Defaults {
///         // default associated type values are stored here
///         type Foo = Option<T>;
///         type __Self = ();
///     }
///
///     // This trait is auto-generated and added as a bound on `Trait` to ensure that
///     // supertraits can only be implemented via the `#[impl_supertrait]` macro.
///     #[doc(hidden)]
///     pub trait SupertraitSealed2484139876 {}
///
///     // This is the actual internal trait that gets generated, including the
///     // auto-generated sealing bound
///     pub trait Trait<T: Copy>: Copy + Sized + SupertraitSealed2484139876 {
///         type Bar;
///
///         fn double_self_plus(&self, plus: Self::Foo) -> (Self, Self, Self::Foo) {
///             (*self, *self, plus)
///         }
///
///         type Foo;
///
///         fn double_value(val: T) -> (T, T) {
///             (val, val)
///         }
///
///         fn triple_value(val: T) -> (T, T, T);
///
///         fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T);
///     }
///
///     // The tokens of this module are all exported via `macro_magic` so that they can be
///     // accessed directly by `#[impl_supertrait]`. This contains all the information
///     // needed to complete the trait impl expansion.
///     #[::supertrait::__private::macro_magic::export_tokens_no_emit(Fizz_exported_tokens)]
///     mod exported_tokens {
///         // tokens for const fns are stored here
///         trait ConstFns {
///             const fn double_value(val: T) -> (T, T) {
///                 (val, val)
///             }
///             const fn triple_value(val: T) -> (T, T, T);
///             const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T);
///         }
///
///         // tokens various versions of the trait generics are stored in these fns
///         fn trait_impl_generics<T: Copy>() {}
///         fn trait_use_generics<T>() {}
///         fn default_impl_generics<T: Copy, __Self>() {}
///         fn default_use_generics<T, __Self>() {}
///
///         // tokens for default associated type values are stored here
///         mod default_items {
///             type Foo = Option<T>;
///         }
///
///         // This const is included solely so `#[impl_supertrait]` can access its `Ident`
///         // to unseal and successfully implement the underlying supertrait on some type.
///         const SupertraitSealed2484139876: () = ();
///     }
/// }
/// ```
///
/// Note that in the macro expansion code, these items are only stored as a token tree within a
/// `macro_rules` macro. Thus the syntax here does not need to compile, it just needs to parse
/// correctly as a module of items from the perspective of `syn`.
///
/// ### Debug Mode
///
/// If you enable the `debug` feature, you can add `debug` as an ident argument to this
/// attribute macro and its expansion will be pretty-printed to the terminal at build time.
/// This is extremely useful for debugging `supertrait` internals and for providing detailed
/// information when reporting bugs.
#[proc_macro_attribute]
pub fn supertrait(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut attr = attr;
    let debug = debug_feature(&mut attr);
    match supertrait_internal(attr, tokens, debug) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

fn debug_feature(attr: &mut TokenStream) -> bool {
    if let Ok(ident) = syn::parse::<Ident>(attr.clone()) {
        if ident == "debug" {
            *attr = TokenStream::new();
            if cfg!(feature = "debug") {
                true
            } else {
                println!("warning: the 'debug' feature must be enabled for debug to work on supertrait attributes.");
                false
            }
        } else {
            false
        }
    } else {
        false
    }
}

fn supertrait_internal(
    attr: impl Into<TokenStream2>,
    tokens: impl Into<TokenStream2>,
    #[allow(unused)] debug: bool,
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
        .push_front(parse_quote!(__Self));
    default_generics
        .use_generics
        .params
        .push_front(parse_quote!(__Self));

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

    // seal trait with random ident
    let random_value: u32 = random();
    let sealed_ident = format_ident!("SupertraitSealed{random_value}");
    let sealed_trait: ItemTrait = parse_quote!(pub trait #sealed_ident {});
    modified_trait.supertraits.push(parse_quote!(#sealed_ident));

    let mut default_remover = DefaultRemover {};
    let mut default_impl_generics_no_defaults = default_impl_generics.clone();
    default_remover.visit_generics_mut(&mut default_impl_generics_no_defaults);

    modified_trait.vis = parse_quote!(pub);

    for def in defaults.iter_mut() {
        def.bounds.clear()
    }

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
                /// Used internally to represent the `Self` type.
                #[doc(hidden)]
                type __Self;
                #(#unfilled_defaults)*
            }

            impl #default_impl_generics_no_defaults DefaultTypes #default_use_generics for Defaults {
                #(#defaults)*
                #[doc(hidden)]
                type __Self = ();
            }

            /// A compile-time generated random trait used to seal supertraits so they can only
            /// be implemented using `impl_supertrait`.
            #[doc(hidden)]
            #sealed_trait

            /// The internal trait that is a part of this supertrait.
            ///
            #(#attrs)*
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

                const #sealed_ident: () = ();
            }
        }
    };
    #[cfg(feature = "debug")]
    if debug {
        output.pretty_print();
    }
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

/// Must be attached to any impl statement involving a supertrait.
///
/// This is the impl analogue of [`#[supertrait]`](`macro@supertrait`) that you should use
/// whenever you impl a supertrait. In fact, a sealing technique is used to prevent anyone from
/// implementing a supertrait manually without the use of `#[impl_supertrait]`, For details on
/// this sealing technique, see the expansion details for
/// [`#[supertrait]`](`macro@supertrait`).
///
/// Consider the following supertrait definition (from the docs for
/// [`#[supertrait]`](`macro@supertrait`)):
///
/// ```ignore
/// use supertrait::*;
///
/// #[supertrait]
/// pub trait Fizz<T: Copy>: Copy + Sized {
///     type Foo = Option<T>;
///     type Bar;
///
///     const fn double_value(val: T) -> (T, T) {
///         (val, val)
///     }
///
///     const fn triple_value(val: T) -> (T, T, T);
///
///     fn double_self_plus(&self, plus: Self::Foo) -> (Self, Self, Self::Foo) {
///         (*self, *self, plus)
///     }
///
///     const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T);
/// }
/// ```
///
/// The following code uses `#[impl_supertrait]` to implement `Fizz<T>` for the struct `Buzz`
/// and makes use of the implementation in various ways:
///
/// ```ignore
/// #[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// struct Buzz;
///
/// #[impl_supertrait]
/// impl<T: Copy> Fizz<T> for Buzz {
///     type Bar = usize;
///
///     const fn triple_value(val: T) -> (T, T, T) {
///         (val, val, val)
///     }
///
///     const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T) {
///         (b, Some(a), a)
///     }
/// }
///
/// #[test]
/// const fn test_buzz_const() {
///     assert!(Buzz::triple_value(3).0 == 3);
///     let buzz = Buzz {};
///     match buzz.interleave('h', false).1 {
///         Some(c) => assert!(c == 'h'),
///         None => unreachable!(),
///     }
/// }
///
/// #[test]
/// fn test_buzz_default_associated_types() {
///     let buzz = Buzz {};
///     assert_eq!(buzz.double_self_plus(Some(3)), (buzz, buzz, Some(3)))
/// }
///```
///
/// Note that the `Self::SomeType` syntax can be used to refer to associated types _anywhere_
/// within a `#[impl_supertrait]` impl block.
///
/// ### Expansion
///
/// Default associated types that are not overridden are redirected to point to their counter
/// parts in `MyTrait::Defaults`, with any accompanying generics. Thus the proper paths of any
/// types mentioned in default associated types is preserved thanks to the `use super::*` in
/// the "wormhole" `MyTrait` module.
///
/// Const fns are implemented as _inherents_ (i.e., directly implemented on the target type)
/// because Rust does not yet support const fns as trait items in stable. This adds a few
/// limitations, mainly around naming collisions, however all bounds and generics on const fns
/// are preserved, and failing to implement them will likewise result in a compile error, so
/// this is probably as close as we can get to something that emulates const fns as trait items
/// in a somewhat usable way in stable Rust.
///
/// Here is the expansion for the above `#[impl_supertrait]`:
///
/// ```ignore
/// impl<T: Copy> Fizz::Trait<T> for Buzz {
///     type Bar = usize;
///
///     // a value for `Foo` was not specified by the implementer, so the macro expansion
///     // substitutes the value for `Foo` contained in `Fizz::Defaults`, preserving
///     // whatever module-local types that may be mentioned in the default associated
///     // type.
///     type Foo = <Fizz::Defaults as Fizz::DefaultTypes<T, Buzz>>::Foo;
///     fn interleave<I>(&self, a: T, b: I) -> (I, <Buzz as Fizz::Trait<T>>::Foo, T) {
///         (b, Some(a), a)
///     }
///     fn triple_value(val: T) -> (T, T, T) {
///         (val, val, val)
///     }
///     fn double_value(val: T) -> (T, T) {
///         (val, val)
///     }
/// }
///
/// // This line unseals `Fizz`. Without this line, the sealing bounds on `Fizz`
/// // will create a compile error.
/// impl Fizz::SupertraitSealed3587271628 for Buzz {}
///
/// // This impl block contains the (inherent) const fn impls of `Fizz` on `Buzz`.
/// impl Buzz {
///     pub const fn interleave<I, T: Copy>(
///         &self,
///         a: T,
///         b: I,
///     ) -> (I, <Buzz as Fizz::Trait<T>>::Foo, T) {
///         (b, Some(a), a)
///     }
///     pub const fn triple_value<T: Copy>(val: T) -> (T, T, T) {
///         (val, val, val)
///     }
///     pub const fn double_value<T: Copy>(val: T) -> (T, T) {
///         (val, val)
///     }
/// }
///
/// // This use statement is automatically inserted by the macro expansion to ensure
/// // the underlying trait is actually brought into scope, since because of the
/// // "wormhole module" pattern, it is actually within the `Fizz` module.
/// #[allow(unused)]
/// use Fizz::Trait as BuzzFizzTraitImpl_4;
/// ```
///
/// See the documentation for [`#[supertrait]`](`macro@supertrait`) for more information.
///
/// ### Debug Mode
///
/// If you enable the `debug` feature, you can add `debug` as an ident argument to this
/// attribute macro and its expansion will be pretty-printed to the terminal at build time.
/// This is extremely useful for debugging `supertrait` internals and for providing detailed
/// information when reporting bugs.
#[proc_macro_attribute]
pub fn impl_supertrait(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut attr = attr;
    let debug = debug_feature(&mut attr);
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
    let debug_tokens = if debug {
        quote!(#[debug_mode])
    } else {
        quote!()
    };
    let output = quote! {
        #[#supertrait_path::__impl_supertrait(#trait_being_impled::#export_tokens_ident)]
        #debug_tokens
        #item_impl
    };
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
    sealed_ident: Ident,
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

        let Some(Item::Const(sealed_const)) = main_body.get(6) else {
            return Err(Error::new(
                item_mod_span,
                "the seventh item in `exported_tokens` should be a const specifying the sealed ident.",
            ));
        };
        let sealed_ident = sealed_const.ident.clone();

        Ok(ImportedTokens {
            const_fns,
            trait_impl_generics: trait_impl_generics.clone(),
            trait_use_generics: trait_use_generics.clone(),
            default_impl_generics: default_impl_generics.clone(),
            default_use_generics: default_use_generics.clone(),
            default_items: default_items,
            sealed_ident,
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

struct ReplaceSelfAssociatedType {
    replace_prefix: TokenStream2,
}

impl VisitMut for ReplaceSelfAssociatedType {
    fn visit_type_path_mut(&mut self, type_path: &mut TypePath) {
        if type_path.path.segments.len() < 2 {
            return;
        }
        let first_seg = type_path.path.segments.first().unwrap();
        if first_seg.ident != "Self" {
            return;
        }
        let segments = type_path.path.segments.iter().skip(1);
        let replace = self.replace_prefix.clone();
        *type_path = parse_quote!(#replace::#(#segments)::*)
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
    #[cfg(feature = "debug")]
    let mut debug = false;
    #[cfg(feature = "debug")]
    for (i, attr) in item_impl.attrs.iter().enumerate() {
        let Some(ident) = attr.path().get_ident() else { continue };
        if ident == "debug_mode" {
            debug = true;
            item_impl.attrs.remove(i);
            break;
        }
    }
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
        default_impl_generics,
        default_use_generics,
        default_items,
        sealed_ident,
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
    let trait_mod_ident = trait_mod.segments.last().unwrap().ident.clone();
    trait_path.segments.insert(
        trait_path.segments.len() - 1,
        parse_quote!(#trait_mod_ident),
    );
    trait_path.segments.last_mut().unwrap().ident = parse_quote!(Trait);

    // strip default generics from default_use_generics
    let mut filtered_tmp = FilteredGenerics {
        impl_generics: default_impl_generics,
        use_generics: default_use_generics,
        has_defaults: HashSet::new(),
    };
    filtered_tmp.strip_default_generics();
    let default_use_generics = filtered_tmp.use_generics;

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
        let mut last_seg = trait_path.segments.last().unwrap().clone();
        last_seg.ident = parse_quote!(Trait);
        let mut visitor = ReplaceSelfAssociatedType {
            replace_prefix: quote!(<#impl_target as #trait_mod::#last_seg>),
        };
        visitor.visit_impl_item_mut(const_fn);
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
        const_fn.sig.generics.params = const_fn
            .sig
            .generics
            .params
            .iter()
            .cloned()
            .map(|g| match g {
                GenericParam::Lifetime(lifetime) => GenericParam::Lifetime(lifetime),
                GenericParam::Type(typ) => {
                    let mut typ = typ.clone();
                    typ.default = None;
                    GenericParam::Type(typ)
                }
                GenericParam::Const(constant) => {
                    let mut constant = constant.clone();
                    constant.default = None;
                    GenericParam::Const(constant)
                }
            })
            .collect();
        const_fn
    });

    item_impl.items.extend(converted_const_fns);
    let mut impl_visitor = FindGenericParam::new(&item_impl.generics);
    impl_visitor.visit_item_impl(&item_impl);
    let mut filtered_generics = filter_generics(&item_impl.generics, &impl_visitor.usages);
    filtered_generics.strip_default_generics();
    item_impl.generics = filtered_generics.impl_generics;

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

        impl #trait_mod::#sealed_ident for #impl_target {}

        #inherent_impl

        #[doc(hidden)]
        #[allow(unused)]
        use #trait_mod::Trait as #trait_import_name;
    };
    #[cfg(feature = "debug")]
    if debug {
        output.pretty_print();
    }
    Ok(output)
}
