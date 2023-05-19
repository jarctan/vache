use super::*;

#[vache_test("24\n")]
fn out_of_scope() -> Program {
    examples::out_of_scope().into()
}
