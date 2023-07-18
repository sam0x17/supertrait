use supertrait::*;

#[supertrait]
pub trait MyTrait {
    fn some_inherent_method(&self) -> u32;
    fn some_method<T: Copy>(something: T) -> T;
    type Something = u32; // default associated type
    const fn something_else() -> usize; // const fn
    type SomethingOverridden = usize; // default associated type that gets overriden
    const SOME_CONSTANT: u8 = 7;
}

struct SomeStruct;

#[impl_supertrait]
impl MyTrait for SomeStruct {
    fn some_method<T: Copy>(something: T) -> T {
        something
    }

    fn some_inherent_method(&self) -> u32 {
        44
    }

    type SomethingOverridden = bool;
}

#[test]
pub fn test_overridden_default_items() {
    SomeStruct::some_method(33);
    SomeStruct::some_method(7);
    let x = SomeStruct;
    assert_eq!(x.some_inherent_method(), 7);
}
