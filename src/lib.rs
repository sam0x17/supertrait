//! # Supertrait
//!
//! [![Crates.io](https://img.shields.io/crates/v/supertrait)](https://crates.io/crates/supertrait)
//! [![docs.rs](https://img.shields.io/docsrs/supertrait?label=docs)](https://docs.rs/supertrait/latest/supertrait/)
//! [![Build Status](https://img.shields.io/github/actions/workflow/status/sam0x17/supertrait/ci.yaml)](https://github.com/sam0x17/supertrait/actions/workflows/ci.yaml?query=branch%3Amain)
//! [![MIT License](https://img.shields.io/github/license/sam0x17/supertrait)](https://github.com/sam0x17/supertrait/blob/main/LICENSE)
//!
//! Supertrait is a revolutionary crate that enables _default associated types_ and _const fn trait
//! items_ in stable Rust as of July 2023. Supertrait accomplishes this through a variety of
//! macro-related techniques including the use of
//! [macro_magic](https://crates.io/crates/macro_magic) as well as the "module wormhole"
//! technique demonstrated in the docs for [`#[supertrait]`](`macro@supertrait`) and
//! [`#[impl_supertrait]`](`macro@impl_supertrait`).
//!
//! Here is an end-to-end example:
//!
//! ```
//! use supertrait::*;
//!
//! #[supertrait]
//! pub trait Fizz<T: Copy>: Copy + Sized {
//!     type Foo = Option<T>;
//!     type Bar;
//!
//!     const fn double_value(val: T) -> (T, T) {
//!         (val, val)
//!     }
//!
//!     const fn triple_value(val: T) -> (T, T, T);
//!
//!     fn double_self_plus(&self, plus: Self::Foo) -> (Self, Self, Self::Foo) {
//!         (*self, *self, plus)
//!     }
//!
//!     const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T);
//! }
//!
//! #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//! struct Buzz;
//!
//! #[impl_supertrait]
//! impl<T: Copy> Fizz<T> for Buzz {
//!     type Bar = usize;
//!
//!     const fn triple_value(val: T) -> (T, T, T) {
//!         (val, val, val)
//!     }
//!
//!     const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T) {
//!         (b, Some(a), a)
//!     }
//! }
//!
//! assert!(Buzz::triple_value(3).0 == 3);
//! let buzz = Buzz {};
//! match buzz.interleave('h', false).1 {
//!     Some(c) => assert!(c == 'h'),
//!     None => unreachable!(),
//! }
//! let buzz = Buzz {};
//! assert_eq!(buzz.double_self_plus(Some(3)), (buzz, buzz, Some(3)));
//! ```
//!
//! Supertraits are also sealed such that a trait created via `#[supertrait]` can only be impled if
//! `#[impl_supertrait]` is attached to the impl statement.
//!
//! Default associated types are implemented in a way that should be nearly identical with how
//! default associated types will function when they are eventually added to stable rust.
//!
//! Const fn trait items are implemented as _inherents_ on the underlying type, however their
//! presence is enforced by `#[impl_supertrait]` and their type bounds are enforced by the
//! requirement for shadow non-const implementations of each const fn trait item that are filled in
//! by the expansion of `#[impl_supertrait]`. These two mechanisms along with the trait sealing
//! technique mentioned above collectively ensure that const fn trait items presence and
//! correctness is enforced just as strongly as that of regular trait items.
//!
//! Using inherents as the vehicle for implementing const fn trait items has a few drawbacks due to
//! the naming collisions that can occur with existing inherent items as well as the inability to
//! blanket impl supertraits containing const fns (because it is impossible in stable Rust to
//! blanket impl anything other than a real trait).
//!
//! That said, inherents are a convenient fallback when you find yourself reaching for const fn
//! items in traits, and supertrait contains the most convenient implementation of this behavior
//! currently possible in stable Rust.

#![no_std]
#![warn(missing_docs)]

pub use supertrait_macros::*;

set_supertrait_path!(crate);

/// Implements [`CustomTypeId`] for the specified type and sets its type id to the specified
/// literal u64 value., e.g. `impl_custom_type_id!(bool, 10681234549081409806);`.
///
/// Specifying that two distinct types have the same `TYPE_ID` can lead to UB, so it is up to
/// the implementer to ensure these remain globally unique.
///
/// Used by [`ConstInto`].
#[macro_export]
macro_rules! impl_custom_type_id {
    ($ty:ty, $val:literal) => {
        impl $crate::CustomTypeId for $ty {
            const TYPE_ID: u64 = $val;
        }
    };
}

/// A trait containing a default assocaited type `TYPE_ID` which is supposed to contain a
/// globally unique `u64` value for all types that implement [`CustomTypeId`].
pub trait CustomTypeId {
    /// An associated const from [`CustomTypeId`] that identifies this type with a globally
    /// unique [`u64`]. Allowing the same two types to have the same `TYPE_ID` is UB.
    const TYPE_ID: u64;
}

impl_custom_type_id!(bool, 10681234549081409806);
impl_custom_type_id!(char, 16675001002995532570);
impl_custom_type_id!(usize, 13672001936412354477);
impl_custom_type_id!(u8, 16098405448592660488);
impl_custom_type_id!(u16, 11966005474853499082);
impl_custom_type_id!(u32, 14174446889875098038);
impl_custom_type_id!(u64, 15791760840720437152);
impl_custom_type_id!(u128, 12185672395767536601);
impl_custom_type_id!(isize, 15162561123529181013);
impl_custom_type_id!(i8, 13119477278634758343);
impl_custom_type_id!(i16, 13226206073516065561);
impl_custom_type_id!(i32, 10624334564060841241);
impl_custom_type_id!(i64, 14258752827364393809);
impl_custom_type_id!(i128, 10725666691061222156);
impl_custom_type_id!(str, 15226379227753928641);
impl_custom_type_id!(&str, 10629156722039909512);

/// This supertrait can be used as a somewhat less useful substitute for being able to make use
/// of `From<T>` / `Into<T>` in const scenarios.
///
/// Because supertrait uses inherent impls as the backend for const fn trait items that are
/// impled, you cannot implement `ConstInto` more than once for the same type.
///
/// Thus the best way to support multiple types is to write a match statement based on
/// `TYPE_ID` and use unsafe pointer dereferencing to handle each of the cases, such as the
/// following:
///
/// ```
/// use supertrait::*;
///
/// pub struct MyStruct {
///     bool: bool,
///     i32: i32,
///     char: char,
/// }
///
/// #[impl_supertrait]
/// impl ConstInto for MyStruct {
///     const fn const_into<T: CustomTypeId>(&self) -> &T {
///         match T::TYPE_ID {
///             bool::TYPE_ID => unsafe { &*((&self.bool as *const bool) as *const T) },
///             i32::TYPE_ID => unsafe { &*((&self.i32 as *const i32) as *const T) },
///             char::TYPE_ID => unsafe { &*((&self.char as *const char) as *const T) },
///             _ => panic!("unsupported type"),
///         }
///     }
/// }
/// ```
#[supertrait]
pub trait ConstInto {
    /// Converts `self` into the specified type. Panics if the specified type is not supported.
    const fn const_into<T: CustomTypeId>(&self) -> &T;
}

/// This supertrait allows you to write const-compatible implementations of [`Clone`].
///
/// A bound is included ensuring that you cannot implement this for types that don't already
/// implement [`Clone`].
///
/// Types that implement [`Copy`] are trivial to support here, since they can be dereferenced
/// to create a copy that can be used as the return result for [`ConstClone`].
#[supertrait]
pub trait ConstClone {
    /// Clones self. Usable in const contexts.
    const fn const_clone(&self) -> Self
    where
        Self: Clone;
}

/// This supertrait allows you to write const-compatible implementations of [`PartialEq`].
///
/// While this cannot be used to replace `==` in const contexts, sometimes it is useful to have
/// analogues of built-in major traits in a const context.
///
/// THe default associated type `Other` defaults to `Self` but can be overridden to determine
/// equality between `self` and other types.
#[supertrait]
pub trait ConstPartialEq {
    /// The type of the item we are comparing `self` with.
    type Other = Self;

    /// Returns `true` if `self` is equal to `other`. Otherwise returns `false`.
    ///
    /// Usable in const contexts.
    const fn const_eq(&self, other: &Self::Other) -> bool;

    /// Returns `false` if `self` is equal to `other`. Otherwise returns `true`.
    ///
    /// Usable in const contexts. Has a default implementation based on the negation of
    /// `const_eq`.
    const fn const_ne(&self, other: &Self::Other) -> bool {
        !self.const_eq(other)
    }
}

set_supertrait_path!(::supertrait);

/// Contains hidden re-exports of [`macro_magic`] and other required types.
#[doc(hidden)]
pub mod __private {
    pub use macro_magic;
}
