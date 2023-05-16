//! Checking that arrays type correctly.

use super::*;

#[test]
fn simple_array_indexing() {
    test(examples::simple_array_indexing(), "lettuce\n");
}

#[test]
fn separate_index_for_array() {
    test(examples::separate_index_for_array(), "zucchini 2\n");
}

#[test]
fn simple_array_assignment() {
    test(examples::simple_array_assignment(), "zucchini 2\n");
}
