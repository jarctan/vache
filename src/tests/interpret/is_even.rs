use super::*;

#[test]
fn is_even() {
    // is 10 even?
    test(examples::is_10_even(), "true\n");

    // is 11 even?
    test(examples::is_11_even(), "false\n");
}
