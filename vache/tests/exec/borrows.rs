use super::*;

#[vache_test("12 10\n")]
fn one_borrow() {
    examples::one_borrow().into()
}

#[vache_test("12 10 10\n")]
fn two_borrows() {
    examples::two_borrows().into()
}
