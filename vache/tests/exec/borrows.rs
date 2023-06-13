use super::*;

#[vache_test("12 10\n")]
fn one_borrow() {
    examples::one_borrow()
}

#[vache_test("12 10 10\n")]
fn two_borrows() {
    examples::two_borrows()
}
