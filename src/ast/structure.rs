use std::collections::HashMap;

use super::Ty;

/// A C-like `struct`.
pub struct Struct {
    /// Name of the structure.
    name: String,
    /// Map of field names and their types.
    fields: HashMap<String, Ty>,
}
