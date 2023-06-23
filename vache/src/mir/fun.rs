//! Defining functions in the MIR.

use std::collections::HashMap;
use std::fmt;

use super::{CfgI, CfgLabel, Pointer, Stratum, VarDef, Varname};
use crate::utils::set::Set;

/// A function in the parser AST.
pub struct Fun<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef<'ctx>>,
    /// Return variable. The variable in which the return value is written.
    ///
    /// If `None`, the function returns nothing.
    pub ret_v: Option<Pointer<'ctx>>,
    /// Map between stratums and their variables.
    pub strata: HashMap<Stratum, Set<Varname<'ctx>>>,
    /// Entry label in the CFG.
    pub entry_l: CfgLabel,
    /// Return label in the CFG.
    pub ret_l: CfgLabel,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: CfgI<'ctx>,
}

impl<'ctx> fmt::Debug for Fun<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name,
            params,
            ret_v,
            strata,
            entry_l,
            ret_l,
            body,
        } = self; // So that if we add a new field, we don;'t forget it here

        let mut res = f.debug_struct(&format!("{name}()"));
        res.field("Parameters", &params);
        if let Some(ret_v) = ret_v {
            res.field("Return variable", &ret_v);
        }
        res.field("Entry", &entry_l)
            .field("Exit", &ret_l)
            .field("Strata", &strata)
            .field("CFG", &body);
        res.finish()
    }
}
