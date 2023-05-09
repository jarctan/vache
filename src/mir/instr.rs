//! Defining instructions, which are the basic units in the MIR.

use std::collections::HashMap;

use super::*;
use crate::utils::boxed;

/// Instructions in the MIR (nodes in the CFG).
pub enum Instr {
    /// Directly goto a label in the CFG.
    Goto(CfgLabel),
    /// Declare a new, uninitialized variable, then goes to a CFG label.
    Declare(VarDef, CfgLabel),
    /// Assigns a variable, then goes to a CFG label.
    Assign(Var, RValue, CfgLabel),
    /// Performs a call to `name(args)`, putting the result in variable
    /// `destination`, and jumping to `target` afterward.
    Call {
        /// Name of the function to call.
        name: String,
        /// Arguments to that function.
        args: Vec<Var>,
        /// Destination variable to hold the result.
        destination: VarDef,
        /// Target label to jump afterward.
        target: CfgLabel,
    },
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: String,
        /// Name of the structure to instantiate.
        fields: HashMap<String, Var>,
        /// Destination variable to hold the instantiated structure.
        destination: VarDef,
        /// Target label to jump afterward.
        target: CfgLabel,
    },
    /// Based on the truthiness of the first argument, jumps either to the 2nd
    /// argument (if true) or the 3rd one (if false)
    Branch(Var, CfgLabel, CfgLabel),
    /// A scoped/nested CFG.
    Scope {
        /// The nested CFG.
        cfg: Cfg,
        /// The entry label into the nested CFG.
        entry_l: CfgLabel,
        /// The exit label of the nested CFG.
        exit_l: CfgLabel,
        /// The target label *in the parent CFG* to jump after traversing that
        /// nested CFG.
        target: CfgLabel,
    },
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Goto(target) => write!(f, "--> {target:?}"),
            Instr::Declare(x, target) => write!(f, "new {x:?} --> {target:?}"),
            Instr::Assign(lhs, rhs, target) => write!(f, "{lhs:?} = {rhs:?} --> {target:?}"),
            Instr::Call {
                name,
                args,
                destination,
                target,
            } => write!(f, "{destination:?} = {name}({args:?}) --> {target:?}"),
            Instr::Struct {
                name,
                fields,
                destination,
                target,
            } => {
                write!(f, "{destination:?} = ")?;
                let mut res = f.debug_struct(name);
                for (name, var) in fields {
                    res.field(name, var);
                }
                res.finish()?;
                write!(f, " --> {target:?}")
            }
            Instr::Branch(cond, iftrue, iffalse) => {
                write!(f, "{cond:?} ? {iftrue:?} : {iffalse:?}")
            }
            Instr::Scope {
                cfg,
                entry_l,
                exit_l,
                target,
            } => write!(f, "{entry_l:?} to {exit_l:?} in {cfg:#?} --> {target:?}"),
        }
    }
}

impl Instr {
    /// Returns the set of (all possible) successors of that label.
    ///
    /// Used for the liveness analysis algorithm, for instance.
    pub fn successors(&self) -> Box<dyn Iterator<Item = CfgLabel>> {
        match self {
            Instr::Goto(target)
            | Instr::Declare(_, target)
            | Instr::Assign(_, _, target)
            | Instr::Call { target, .. }
            | Instr::Struct { target, .. }
            | Instr::Scope { target, .. } => boxed(std::iter::once(target.clone())),
            Instr::Branch(_, iftrue, iffalse) => {
                boxed([iftrue.clone(), iffalse.clone()].into_iter())
            }
        }
    }
}
