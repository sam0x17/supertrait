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

pub struct MyStruct {
    bool: bool,
    i32: i32,
    char: char,
}

#[impl_supertrait]
impl ConstInto for MyStruct {
    const fn const_into<T: CustomTypeId>(&self) -> &T {
        match T::TYPE_ID {
            bool::TYPE_ID => unsafe { &*((&self.bool as *const bool) as *const T) },
            i32::TYPE_ID => unsafe { &*((&self.i32 as *const i32) as *const T) },
            char::TYPE_ID => unsafe { &*((&self.char as *const char) as *const T) },
            _ => panic!("unsupported type"),
        }
    }
}

#[test]
const fn test_const_into() {
    let s = MyStruct {
        bool: true,
        i32: -67,
        char: 'h',
    };
    assert!(*s.const_into::<bool>() == true);
    assert!(*s.const_into::<i32>() == -67);
    assert!(*s.const_into::<char>() == 'h');
}
