//! Parsing structs, and defining their representation in the AST.

use std::collections::HashMap;

use super::Ty;

/// A C-like `struct`.
#[derive(Debug, Clone)]
pub struct Struct<'ctx> {
    /// Name of the structure.
    pub name: &'ctx str,
    /// Map of field names and their types.
    pub fields: HashMap<&'ctx str, Ty<'ctx>>,
}

impl<'ctx> Struct<'ctx> {
    /// Gets the type of a field in the structure.
    pub fn get_field(&self, field: impl AsRef<str>) -> &Ty<'ctx> {
        &self.fields[field.as_ref()]
    }
}
