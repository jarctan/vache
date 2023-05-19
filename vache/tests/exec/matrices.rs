use super::*;

#[vache_test("42\n")]
fn simple_matrix_modify() -> Program {
    examples::simple_matrix_modify().into()
}
