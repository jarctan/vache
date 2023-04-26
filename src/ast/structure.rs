use std::collections::HashMap;

use super::Ty;

/// A C-like `struct`.
#[derive(Debug, Clone)]
pub struct Struct {
    /// Name of the structure.
    pub name: String,
    /// Map of field names and their types.
    pub fields: HashMap<String, Ty>,
}

impl Struct {
    /// Gets the type of a field in the structure.
    pub fn get_field(&self, field: impl AsRef<str>) -> &Ty {
        &self.fields[field.as_ref()]
    }
}
