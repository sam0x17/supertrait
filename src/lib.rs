#![no_std]

pub use supertrait_macros::*;

set_supertrait_path!(crate);

#[macro_export]
macro_rules! impl_custom_type_id {
    ($ty:ty, $val:literal) => {
        impl $crate::CustomTypeId for $ty {
            const TYPE_ID: u64 = $val;
        }
    };
}

pub trait CustomTypeId {
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

#[supertrait]
pub trait IntoConst {
    const fn into_const<T: CustomTypeId>(&self) -> &T;
}

set_supertrait_path!(::supertrait);

#[doc(hidden)]
pub mod __private {
    pub use macro_magic;
}
