use supertrait::*;

#[supertrait]
pub trait MyTrait<T: Copy, I: Copy> {
    fn some_method(something: T) -> T;
    type Something = I; // default associated type
    const fn something_else() -> usize; // const fn
    type SomethingOverridden = usize; // default associated type that gets overriden
}

// #[supertrait]
// pub trait MyTrait<T: Copy, I: Copy> {
//     fn some_method(something: T) -> T;
//     type Something = I; // default associated type
// }
// pub struct SomeStruct;

struct SomeStruct;

#[impl_supertrait]
impl<T: Copy, I: Copy> MyTrait for SomeStruct {
    fn some_method(something: T) -> T {
        something
    }
}
