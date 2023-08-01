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

#[test]
const fn test_default_generics_b() {
    assert!(TestingDefaultGenericsOverridden::something_using_t(-89i64) == -89i64);
}

#[supertrait]
pub trait DefaultGenericsOverriddenSpecific<T = bool> {
    type Something = T;

    const fn something_using_t(val: T) -> T;
}

struct TestingDefaultGenericsOverriddenSpecific;

#[impl_supertrait]
impl DefaultGenericsOverriddenSpecific<i32> for TestingDefaultGenericsOverriddenSpecific {
    type Something = u8;

    const fn something_using_t(val: i32) -> i32 {
        val
    }
}

#[test]
const fn test_default_generics_c() {
    assert!(TestingDefaultGenericsOverriddenSpecific::something_using_t(-33) == -33);
    let _a: <TestingDefaultGenericsOverriddenSpecific as DefaultGenericsOverriddenSpecific::Trait<i32>>::Something = 5u8;
}
