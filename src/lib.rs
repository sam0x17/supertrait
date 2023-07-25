#![no_std]

pub use supertrait_macros::*;

set_supertrait_path!(crate);

pub trait CustomTypeId {
    const TYPE_ID: usize;
}

impl CustomTypeId for bool {
    const TYPE_ID: usize = 0;
}

impl CustomTypeId for u8 {
    const TYPE_ID: usize = 1;
}

pub struct MyStruct;

#[supertrait]
pub trait IntoConst<U> {
    const fn into_const<T: CustomTypeId>(&self) -> U;
}

set_supertrait_path!(::supertrait);

#[doc(hidden)]
pub mod __private {
    pub use macro_magic;
}
