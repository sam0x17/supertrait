use supertrait::*;

#[supertrait]
pub trait MyTrait {
    fn some_inherent_method(&self) -> u32;
    type Something = u32; // default associated type
    const fn yet_another_thing() -> bool; // const fn
    const fn something_else(&self) -> usize; // const fn self
    type SomethingOverridden = usize; // default associated type that gets overridden
    const SOME_CONSTANT: u8 = 7;
}

struct SomeStruct;

#[impl_supertrait]
impl MyTrait for SomeStruct {
    fn some_inherent_method(&self) -> u32 {
        44
    }

    type SomethingOverridden = bool;

    const fn yet_another_thing() -> bool {
        false
    }

    const fn something_else(&self) -> usize {
        178
    }
}

#[test]
fn test_default_associated_types() {
    let x = SomeStruct;
    assert_eq!(x.some_inherent_method(), 44);
    let _y: <SomeStruct as MyTrait::Trait>::Something = 7;
    let _z: <SomeStruct as MyTrait::Trait>::SomethingOverridden = false;
    assert_eq!(<SomeStruct as MyTrait::Trait>::SOME_CONSTANT, 7);
}

#[test]
fn test_const_fn_trait_items() {
    const _TEST_CONST_FN: bool = SomeStruct::yet_another_thing();
    const _SOME_STRUCT: SomeStruct = SomeStruct {};
    const _TEST_CONST_SELF_FN: usize = _SOME_STRUCT.something_else();
}

#[supertrait]
pub trait IntoConstU8 {
    const fn into_const_u8(&self) -> u8;
}

#[supertrait]
pub trait IntoConstBool {
    const fn into_const_bool(&self) -> bool;
}

pub struct MyStruct;

#[impl_supertrait]
impl IntoConstU8 for MyStruct {
    const fn into_const_u8(&self) -> u8 {
        37
    }
}

#[impl_supertrait]
impl IntoConstBool for MyStruct {
    const fn into_const_bool(&self) -> bool {
        false
    }
}

pub trait IntoConstUnion {}

pub union SupportedTypes {
    bool: bool,
    u8: u8,
}

impl IntoConstUnion for SupportedTypes {}

#[supertrait]
pub trait IntoConst<U: IntoConstUnion> {
    const fn into_const<T>(&self) -> U;
}

// #[impl_supertrait]
// impl IntoConst<SupportedTypes> for MyStruct {
//     const fn into_const<T>(&self) -> SupportedTypes {
//         core::any::type_name::<T>();
//     }
// }

// #[supertrait]
// pub trait IntoConst {
//     const
// }

// pub trait IntoProvider<T> {
//     type Inherent;
// }

// #[supertrait]
// pub trait ConstInto<T>: IntoProvider<T> {
//     const fn into_const(&self) -> T;
// }

// pub struct AnotherStruct;

// pub struct SomeRandomThrowaway;

// impl SomeRandomThrowaway {
//     pub const fn into_const(&self) -> u8 {
//         34
//     }
// }

// impl IntoProvider<u8> for AnotherStruct {
//     type Inherent = SomeRandomThrowaway;
// }

// #[impl_supertrait]
// impl ConstInto<u8> for AnotherStruct {
//     const fn into_const(&self) -> u8 {
//         let x: <Self as IntoProvider<u8>>::Inherent = SomeRandomThrowaway {};
//         x.into_const()
//     }
// }

// #[impl_supertrait]
// impl ConstInto<bool> for AnotherStruct {
//     const fn into_const(&self) -> bool {
//         false
//     }
// }
