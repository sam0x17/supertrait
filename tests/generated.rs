use supertrait::*;

#[supertrait]
pub trait DefaultedType {
    type Output: Default = i32;
    fn default() -> Self::Output;
}

// #[supertrait]
// pub trait ConstFn {
//     const SIZE: usize;
//     fn const_size() -> usize;
// }

// #[supertrait]
// pub trait SuperTrait: DefaultedType + ConstFn {}

// // impl_supertraits.rs

// #[impl_supertrait]
// pub struct TypeWithDefault;
// impl DefaultedType for TypeWithDefault {
//     fn default() -> <Self as DefaultedType>::Output {
//         0
//     }
// }

// #[impl_supertrait]
// pub struct TypeWithConst;
// impl ConstFn for TypeWithConst {
//     const SIZE: usize = 10;
//     fn const_size() -> usize {
//         Self::SIZE
//     }
// }

// #[impl_supertrait]
// pub struct SuperType;
// impl DefaultedType for SuperType {
//     fn default() -> <Self as DefaultedType>::Output {
//         0
//     }
// }
// impl ConstFn for SuperType {
//     const SIZE: usize = 10;
//     fn const_size() -> usize {
//         Self::SIZE
//     }
// }
// impl SuperTrait for SuperType {}

// // tests.rs

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_defaulted_type_generic() {
//         assert_eq!(<TypeWithDefault as DefaultedType>::default(), 0);
//     }

//     #[test]
//     fn test_const_fn_generic() {
//         assert_eq!(<TypeWithConst as ConstFn>::const_size(), 10);
//     }

//     #[test]
//     fn test_super_trait_generic() {
//         assert_eq!(<SuperType as DefaultedType>::default(), 0);
//         assert_eq!(<SuperType as ConstFn>::const_size(), 10);
//     }
// }
