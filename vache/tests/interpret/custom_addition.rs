use super::*;

#[vache_test("24 12\n")]
fn custom_addition() -> Program {
    examples::custom_addition().into()
}
