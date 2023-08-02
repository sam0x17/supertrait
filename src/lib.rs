//! # Supertrait 🦹
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
pub mod traits;

/// Contains hidden re-exports of [`macro_magic`] and other required types.
#[doc(hidden)]
pub mod __private {
    pub use macro_magic;
}
