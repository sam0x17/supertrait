use supertrait::*;

#[supertrait]
pub trait MyTrait {
    fn some_inherent_method(&self) -> u32;
    type Something = u32; // default associated type
    const fn something_else() -> usize; // const fn
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

    const fn something_else() -> usize {
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
    const _TEST_CONST: usize = SomeStruct::something_else();
}
