use supertrait::*;

#[supertrait]
pub trait DefaultGenericsOverridden<T = bool> {
    type Something = T;

    const fn something_using_t(val: T) -> T;
}

struct TestingDefaultGenericsOverridden;

#[impl_supertrait]
impl<T> DefaultGenericsOverridden<T> for TestingDefaultGenericsOverridden {
    type Something = u8;

    const fn something_using_t(val: T) -> T {
        val
    }
}
