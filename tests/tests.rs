#![no_std]

use supertrait::traits::*;
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
const fn test_const_fn_trait_items() {
    SomeStruct::yet_another_thing();
    SomeStruct {}.something_else();
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct WrappedU8(u8);

impl From<u8> for WrappedU8 {
    fn from(value: u8) -> Self {
        WrappedU8(value)
    }
}

impl From<WrappedU8> for u8 {
    fn from(value: WrappedU8) -> Self {
        value.0
    }
}

#[supertrait]
pub trait RIntoWithDATs<'i, 'o, O: PartialEq + 'o>
where
    Self: Into<O>,
    O: Into<Self>,
{
    type InputRef = Self;
    type OutputRef = &'o O;
    type InputOption = Option<Self>;
    type OutputOption = Option<O>;
    type Unspecified;
}

#[impl_supertrait]
impl<'i, 'o> RIntoWithDATs<'i, 'o, WrappedU8> for u8 {
    type Unspecified = (usize, usize);
    type OutputRef = &'o bool;
}

#[test]
fn test_generics_in_default_associated_types() {
    let _a: <u8 as RIntoWithDATs::Trait<WrappedU8>>::Unspecified = (1, 2);
    let _b: <u8 as RIntoWithDATs::Trait<WrappedU8>>::OutputRef = &false;
    let _c: <u8 as RIntoWithDATs::Trait<WrappedU8>>::OutputOption = None;
    let _b: <u8 as RIntoWithDATs::Trait<WrappedU8>>::InputRef = 3u8;
}

#[impl_supertrait]
impl ConstClone for WrappedU8 {
    const fn const_clone(&self) -> Self
    where
        Self: Clone,
    {
        *self
    }
}

#[test]
const fn test_const_clone() {
    let a = WrappedU8(3);
    let _b: WrappedU8 = a.const_clone();
    assert!(_b.0 == 3);
}

#[impl_supertrait]
impl ConstPartialEq for WrappedU8 {
    const fn const_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[test]
const fn test_const_partial_eq() {
    let a = WrappedU8(1);
    let b = WrappedU8(2);
    let c = WrappedU8(1);
    assert!(a.const_ne(&b));
    assert!(a.const_eq(&c));
}

// intentionally incorrect implementation
#[impl_supertrait]
impl ConstPartialEq for MyStruct {
    const fn const_eq(&self, _other: &Self) -> bool {
        false
    }

    const fn const_ne(&self, _other: &Self) -> bool {
        false
    }
}

#[test]
const fn test_incorrect_supertrait() {
    let a = MyStruct {
        bool: true,
        i32: 3,
        char: 'a',
    };
    let b = MyStruct {
        bool: true,
        i32: 3,
        char: 'a',
    };
    assert!(!a.const_eq(&b));
    assert!(!a.const_ne(&b));
}

#[supertrait]
pub trait ConstTraitWithGenerics<T: Copy, I: Copy> {
    type Something: Copy;
    type SomethingWithDefault = u8;

    const fn something_using_i() -> I;
    const fn something_using_t(val: T) -> T;
    const fn something_using_something() -> Self::Something;
    fn something() -> Self::Something;
    const fn something_using_something_with_default(
        val: Self::SomethingWithDefault,
    ) -> Self::SomethingWithDefault;
}

#[impl_supertrait]
impl<T: Copy> ConstTraitWithGenerics<T, i32> for MyStruct {
    type Something = u64;

    const fn something_using_i() -> i32 {
        44
    }

    const fn something_using_t(val: T) -> T {
        val
    }

    const fn something_using_something() -> Self::Something {
        47
    }

    fn something() -> Self::Something {
        33
    }

    const fn something_using_something_with_default(
        val: Self::SomethingWithDefault,
    ) -> Self::SomethingWithDefault {
        val
    }
}

#[test]
const fn test_const_trait_generics() {
    assert!(MyStruct::something_using_something::<bool>() == 47u64);
    assert!(MyStruct::something_using_i() == 44i32);
    assert!(MyStruct::something_using_t::<isize>(-33) == -33);
    assert!(MyStruct::something_using_something_with_default::<i128>(23u8) == 23u8);
}

#[supertrait]
pub trait Fizz<T: Copy>: Copy + Sized {
    type Foo = Option<T>;
    type Bar;

    const fn double_value(val: T) -> (T, T) {
        (val, val)
    }

    const fn triple_value(val: T) -> (T, T, T);

    fn double_self_plus(&self, plus: Self::Foo) -> (Self, Self, Self::Foo) {
        (*self, *self, plus)
    }

    const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T);
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct Buzz;

#[impl_supertrait]
impl<T: Copy> Fizz<T> for Buzz {
    type Bar = usize;

    const fn triple_value(val: T) -> (T, T, T) {
        (val, val, val)
    }

    const fn interleave<I>(&self, a: T, b: I) -> (I, Self::Foo, T) {
        (b, Some(a), a)
    }
}

#[test]
const fn test_buzz_const() {
    assert!(Buzz::triple_value(3).0 == 3);
    let buzz = Buzz {};
    match buzz.interleave('h', false).1 {
        Some(c) => assert!(c == 'h'),
        None => unreachable!(),
    }
}

#[test]
fn test_buzz_default_associated_types() {
    let buzz = Buzz {};
    assert_eq!(buzz.double_self_plus(Some(3)), (buzz, buzz, Some(3)))
}

pub mod some_mod {
    pub mod some_sub_mod {
        pub mod sub_sub {
            pub trait Something {}
        }
        pub struct Wiz;
        #[supertrait::supertrait]
        pub trait TraitCapturingLocalType: sub_sub::Something {
            const fn pass_something<T: sub_sub::Something>(val: T) -> T;
            fn some_method<T: sub_sub::Something>(val: T) -> T;
            type SomeType = Wiz;
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
struct Fred;

impl some_mod::some_sub_mod::sub_sub::Something for Fred {}

#[impl_supertrait]
impl some_mod::some_sub_mod::TraitCapturingLocalType for Fred {
    const fn pass_something<T: some_mod::some_sub_mod::sub_sub::Something>(val: T) -> T {
        val
    }

    fn some_method<T: some_mod::some_sub_mod::sub_sub::Something>(val: T) -> T {
        val
    }
}

#[test]
fn test_wormhole_module_teleportation() {
    const A: Fred = Fred::pass_something(Fred {});
    let b: Fred = Fred::some_method(Fred {});
    assert_eq!(A, b);
}

#[supertrait]
pub trait TraitWithDefaultGenerics<T = bool> {
    type Something = T;

    const fn something_using_t(val: T) -> T;
}

struct TestingTraitWithDefaultGenerics;

#[impl_supertrait(debug)]
impl<T> TraitWithDefaultGenerics for TestingTraitWithDefaultGenerics {
    const fn something_using_t(val: bool) -> bool {
        true
    }
}
