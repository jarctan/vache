//! Defining the diagnostics error codes.

/// Unknown variable name.
pub const UNKNOWN_VAR_ERROR: &str = "E0001";
/// Heterogeneous lists error.
pub const HETEROGENEOUS_LISTS_ERROR: &str = "E0002";
/// Type mismatch between expected and found.
pub const TYPE_MISMATCH_ERROR: &str = "E0003";
/// No such field error.
pub const FIELD_ACCESS_ERROR: &str = "E0004";
/// Struct instantiation error.
pub const STRUCT_INSTANCE_ERROR: &str = "E0005";
/// Parser error.
pub const PARSER_ERROR: &str = "E0006";
/// Error for items defined several times.
pub const ITEM_REDEFINED_ERROR: &str = "E0007";
/// Unknown identifier.
pub const UNKNOWN_IDENT_ERROR: &str = "E0008";
/// Argument number mismatch.
pub const ARG_NB_MISMATCH: &str = "E0009";
/// Cannot be called.
pub const NOT_CALLABLE_ERROR: &str = "E0010";
/// Unknown type variable.
pub const UNKNOWN_TYPE_VAR: &str = "E0011";
/// Access the field of something that is not a struct.
pub const FIELD_NOT_STRUCT_ERROR: &str = "E0012";
/// Not a valid pattern.
pub const INVALID_PATTERN: &str = "E0013";
/// Error when something that is not a place is borrowed mutably.
pub const AS_MUT_NOT_PLACE_ERROR: &str = "E0014";
/// Requiring an action that can only be executed within loops, but we are not
/// in one.
pub const NOT_IN_LOOP_ERROR: &str = "E0015";
/// Tuple access error.
pub const TUPLE_ACCESS_ERROR: &str = "E0016";
/// Temporary: empty list error.
pub const EMPTY_LIST_ERROR: &str = "E0100";
