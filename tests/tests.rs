use supertrait::*;

#[supertrait]
pub trait MyTrait {
    fn some_inherent_method(&self) -> u32;
    fn some_method<T>(something: T) -> T;
    type Something = u32; // default associated type
    const fn something_else() -> usize; // const fn
    type SomethingOverridden = usize; // default associated type that gets overridden
    const SOME_CONSTANT: u8 = 7;
}

struct SomeStruct;

#[impl_supertrait]
impl MyTrait for SomeStruct {
    fn some_method<T>(something: T) -> T {
        something
    }

    fn some_inherent_method(&self) -> u32 {
        44
    }

    type SomethingOverridden = bool;
}

#[test]
pub fn test_default_associated_types() {
    SomeStruct::some_method(33);
    SomeStruct::some_method("hello");
    let x = SomeStruct;
    assert_eq!(x.some_inherent_method(), 44);
    let _y: <SomeStruct as MyTrait::Trait>::Something = 7;
    let _z: <SomeStruct as MyTrait::Trait>::SomethingOverridden = false;
    assert_eq!(<SomeStruct as MyTrait::Trait>::SOME_CONSTANT, 7);
}
