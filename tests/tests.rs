#![no_std]

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

pub trait CustomTypeId {
    const TYPE_ID: usize;
}

pub union MyStructInto {
    bool: bool,
    u8: u8,
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

#[impl_supertrait]
impl IntoConst<MyStructInto> for MyStruct {
    const fn into_const<T: CustomTypeId>(&self) -> MyStructInto {
        match T::TYPE_ID {
            bool::TYPE_ID => MyStructInto { bool: true },
            u8::TYPE_ID => MyStructInto { u8: 33 },
            _ => panic!("invalid"),
        }
    }
}
