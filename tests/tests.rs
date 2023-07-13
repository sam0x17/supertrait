use supertrait::*;

#[supertrait]
trait MyTrait {
    fn some_method() -> usize;
    type Something = usize; // default associated type
    const fn something_else() -> usize; // const fn
}
