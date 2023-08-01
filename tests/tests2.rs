use supertrait::*;

pub trait NormalTrait<T = bool> {
    type Something;
    fn something_using_t(val: T) -> T;
}

struct NormalThing;

impl<T> NormalTrait<T> for NormalThing {
    type Something = bool;

    fn something_using_t(val: T) -> T {
        val
    }
}

#[supertrait]
pub trait TraitWithDefaultGenerics<T = bool> {
    type Something = T;

    const fn something_using_t(val: T) -> T;
}

struct TestingTraitWithDefaultGenerics;

#[impl_supertrait]
impl TraitWithDefaultGenerics for TestingTraitWithDefaultGenerics {
    const fn something_using_t(val: bool) -> bool {
        val
    }
}
