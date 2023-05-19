use super::*;

#[vache_test("true\n")]
fn is_10_even() -> Program {
    examples::is_10_even().into()
}

#[vache_test("false\n")]
fn is_11_even() -> Program {
    examples::is_11_even().into()
}
