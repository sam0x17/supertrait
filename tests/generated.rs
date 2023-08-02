//! These tests were generated by Chat GPT 4

use supertrait::*;

#[supertrait(debug)]
pub trait DefaultedType {
    type Output: Default = i32;
    fn default() -> Self::Output;
}

#[supertrait]
pub trait ConstFn {
    const SIZE: usize;
    fn const_size() -> usize;
}

#[supertrait]
pub trait SuperTrait: DefaultedType::Trait + ConstFn::Trait {}

pub struct TypeWithDefault;

#[impl_supertrait]
impl DefaultedType for TypeWithDefault {
    fn default() -> Self::Output {
        0
    }
}

pub struct TypeWithConst;

#[impl_supertrait]
impl ConstFn for TypeWithConst {
    const SIZE: usize = 10;
    fn const_size() -> usize {
        Self::SIZE
    }
}

pub struct SuperType;

#[impl_supertrait]
impl DefaultedType for SuperType {
    fn default() -> Self::Output {
        0
    }
}

#[impl_supertrait]
impl ConstFn for SuperType {
    const SIZE: usize = 10;
    fn const_size() -> usize {
        Self::SIZE
    }
}

#[impl_supertrait]
impl SuperTrait for SuperType {}

#[test]
fn test_defaulted_type_generic() {
    assert_eq!(<TypeWithDefault as DefaultedType::Trait>::default(), 0);
}

#[test]
fn test_const_fn_generic() {
    assert_eq!(TypeWithConst::const_size(), 10);
}

#[test]
fn test_super_trait_generic() {
    assert_eq!(SuperType::default(), 0);
    assert_eq!(SuperType::const_size(), 10);
}
