//! Defining typed functions.

use std::collections::HashMap;

use super::{Block, FunParam, Pointer, Stratum, Varname};
use crate::utils::Set;

/// A function in the typed AST.
#[derive(Debug)]
pub struct Fun<'mir, 'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<FunParam<'ctx>>,
    /// Return variable. The variable in which the return value is written.
    ///
    /// If `None`, the function returns nothing.
    pub ret_v: Option<Pointer<'ctx>>,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block<'mir, 'ctx>,
    /// Map between stratums and their variables.
    pub strata: HashMap<Stratum, Set<Varname<'ctx>>>,
}
