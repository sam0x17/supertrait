# Supertrait

[![Crates.io](https://img.shields.io/crates/v/supertrait)](https://crates.io/crates/supertrait)
[![docs.rs](https://img.shields.io/docsrs/supertrait?label=docs)](https://docs.rs/supertrait/latest/supertrait/)
[![Build Status](https://img.shields.io/github/actions/workflow/status/sam0x17/supertrait/ci.yaml)](https://github.com/sam0x17/supertrait/actions/workflows/ci.yaml?query=branch%3Amain)
[![MIT License](https://img.shields.io/github/license/sam0x17/supertrait)](https://github.com/sam0x17/supertrait/blob/main/LICENSE)

Supertrait is a revolutionary crate that enables _default associated types_ and _const fn trait
items_ in stable Rust as of July 2023. Supertrait accomplishes these features through a variety
of macro-related techniques including the use of
[macro_magic](https://crates.io/crates/macro_magic) as well as the "module wormhole" technique
demonstrated in the docs and expansion for `#[supertrait]` and `#[impl_supertrait]`.

Supertraits are also sealed such that a trait created via `#[supertrait]` _can only be impled_
if `#[impl_supertrait]` is attached to the impl statement.

Default associated types are implemented in a way that should be nearly identical with how
default associated types will functions when they are eventually added to stable rust.

Const fn trait items are implemented as _inherents_ on the underlying type, however their
presence is enforced by `#[impl_supertrait]` and their type bounds are enforced by the
requirement for shadow non-const implementations of each const fn trait item that are filled in
by the expansion of `#[impl_supertrait]`. These two mechanisms along with the trait sealing
technique mentioned above collectively ensure that const fn trait items presence and
correctness is enforced just as strongly as that of regular trait items.

Using inherents as the vehicle for implementing const fn trait items has a few
drawbacks due to the naming collisions that can occur with existing inherent items as well as
the inability to blanket impl supertraits containing const fns because it is impossible in
stable Rust to blanket impl anything other than a real trait (you can'd do this with
inherents).

That said, inherents are a convenient fallback when you find yourself reaching for const fn
items in traits, and supertrait contains the most convenient implementation of this behavior
currently possible in stable Rust.
