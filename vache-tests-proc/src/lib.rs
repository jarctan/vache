//! Vache crate for procedural macros.
//!
//! In accordance with Rust directives for procedural macros, we define all
//! procedural macros that we need for the Vache compiler in a separate crate.
//! This includes procedural macros used by test functions.
//!
//! Each module stands for a procedural macro. The principle is always the same:
//! define two structures that list all the information that we need to get from
//! the Rust tokens. One is the information in the attribute, and the other is
//! the information that we get from the element itself on which the
//! macro is applied.

#![warn(missing_docs)]

#[macro_use]
extern crate quote;

extern crate proc_macro;

mod parses;
mod should_fail;
mod vache_test;

use proc_macro::TokenStream;

/// Parses a source code in the form of a string, and gives you the result as
/// the first argument of a function.
///
/// Format of the macro: `#[parses(source_code as rule)]` where:
/// * `source_code` is a literal string (**not** an expression)
/// * rule is a valid rule name from the pest grammar
///
/// The macro must be applied to some function that takes only one argument,
/// whose type is the desired type of the parsed expression.
///
/// Warning: because it will use `crate`, this macro can only be used directly
/// within the main crate.
///
/// Example:
/// ```rust,ignore
/// #[parses("()" as ty)]
/// #[test]
/// fn unit_ty(ty: TyUse) {
///     assert_eq!(ty, UnitT);
/// }
/// ```
#[proc_macro_attribute]
pub fn parses(attr: TokenStream, item: TokenStream) -> TokenStream {
    parses::parses(attr, item)
}

/// Runs the Vache program returned by the body of your function, and compares
/// the output to the provided, expected output.
///
/// Format of the macro: `#[vache_test(expected_output)]` where:
/// * `expected_output` is a literal string (**not** an expression)
///
/// The macro must be applied to some function that does _NOT_ take any
/// argument, and which necessarily returns a `Program`.
///
/// Warning: because it will import `vache_lib`, this macro cannot be used
/// directly within the main crate (only in the tests/examples crate for
/// instance).
///
/// Example:
/// ```rust,ignore
/// #[vache_test("lettuce\n")]
/// fn simple_array_indexing() -> Program {
///     examples::simple_array_indexing().into()
/// }
/// ```
#[proc_macro_attribute]
pub fn vache_test(attr: TokenStream, item: TokenStream) -> TokenStream {
    vache_test::vache_test(attr, item)
}

/// Type-checks the Vache program returned by the body of your function, and
/// checks that it does NOT type-check and provides the same error codes as
/// those expected.
///
/// Format of the macro: `#[should_fail(error_code_list)]` where:
/// * `expected_output` is a comma-separated list of error codes (expressions
///   are allowed, but should resolve to strings).
///
/// The macro must be applied to some function that does _NOT_ take any
/// argument, and which necessarily returns a `Program`.
///
/// Warning: because it will import `vache_lib`, this macro cannot be used
/// directly within the main crate (only in the tests/examples crate for
/// instance).
///
/// Example:
/// ```rust,ignore
/// /// We assign to a variable `e` another variable `d` which is already out of
/// scope.
/// #[should_fail(UNKNOWN_VAR_ERROR)]
/// #[test]
/// fn wrong_nested_scopes() -> Program {
///     /* Your program */
/// }
/// ```
#[proc_macro_attribute]
pub fn should_fail(attr: TokenStream, item: TokenStream) -> TokenStream {
    should_fail::should_fail(attr, item)
}
