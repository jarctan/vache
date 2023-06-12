//! Defining the diagnostics error codes.

/// Unknown variable name.
pub const UNKNOWN_VAR_ERROR: &str = "E0001";
/// Unknown structure name.
pub const UNKNOWN_STRUCT_ERROR: &str = "E0001";
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
/// Temporary: empty list error.
pub const EMPTY_LIST_ERROR: &str = "E0100";
