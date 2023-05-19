//! Checking that arrays type correctly.

use super::*;

#[vache_test("lettuce\n")]
fn simple_array_indexing() -> Program {
    examples::simple_array_indexing().into()
}

#[vache_test("zucchini 2\n")]
fn separate_index_for_array() -> Program {
    examples::separate_index_for_array().into()
}

#[vache_test("cucumber 2\n")]
fn simple_array_assignment() -> Program {
    examples::simple_array_assignment().into()
}

#[vache_test("cucumber zucchini 2\n")]
fn assignment_while_borrowed() -> Program {
    examples::assignment_while_borrowed().into()
}

#[vache_test("cucumber cabbage\n")]
fn cloning_array() -> Program {
    examples::cloning_array().into()
}
