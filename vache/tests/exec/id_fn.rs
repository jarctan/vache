use super::*;

#[vache_test("12\n")]
fn id_fn() -> Program {
    examples::id_fn()
}
