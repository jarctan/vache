use super::*;

#[vache_test("dupont doe\n")]
fn change_field() -> Program {
    examples::change_field().into()
}
