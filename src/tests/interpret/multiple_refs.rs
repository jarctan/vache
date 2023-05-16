use super::*;

/// Check twice if 10 is even, just to be sure.
/// One never knows, maybe it changed its mind?
///
/// This test is to prove that you can reuse a variable multiple times.
#[test]
fn multiple_refs() {
    test(examples::multiple_refs(), "true true false\n");
}
