use super::*;

/// Check twice if 10 is even, just to be sure.
/// One never knows, maybe it changed its mind?
///
/// This test is to prove that you can reuse a variable multiple times.

#[vache_test("true true false\n")]
fn multiple_refs() -> Program {
    examples::multiple_refs().into()
}
