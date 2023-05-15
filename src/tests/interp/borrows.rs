use super::*;

#[test]
fn one_borrow() {
    test(examples::one_borrow(), "12 10\n");
}

#[test]
fn two_borrows() {
    test(examples::two_borrows(), "12 10 10\n");
}
