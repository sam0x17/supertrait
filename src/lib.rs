#![no_std]

pub use supertrait_macros::*;

set_supertrait_path!(crate);

#[supertrait]
pub trait CustomTypeId {
    const TYPE_ID: usize;
}

#[impl_supertrait]
impl CustomTypeId for bool {
    const TYPE_ID: usize = 0;
}

#[impl_supertrait]
impl CustomTypeId for u8 {
    const TYPE_ID: usize = 1;
}

#[supertrait]
pub trait IntoConst<U> {
    const fn into_const<T: CustomTypeId::Trait>(&self) -> U;
}

set_supertrait_path!(::supertrait);

#[doc(hidden)]
pub mod __private {
    pub use macro_magic;
}
