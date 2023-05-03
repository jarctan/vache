//! Defining instructions, which are the basic units in the MIR.

use std::collections::HashMap;

use super::*;

#[derive(Clone)]
pub enum Instr {
    Goto(CfgLabel),
    Declare(VarDef, CfgLabel),
    Assign(Var, RValue, CfgLabel),
    Call {
        name: String,
        args: Vec<Var>,
        destination: VarDef,
        target: CfgLabel,
    },
    Struct {
        name: String,
        fields: HashMap<String, Var>,
        destination: VarDef,
        target: CfgLabel,
    },
    Branch(Var, CfgLabel, CfgLabel),
    Scope {
        cfg: Cfg,
        entry_l: CfgLabel,
        exit_l: CfgLabel,
        target: CfgLabel,
    },
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Goto(target) => write!(f, "--> {target:?}"),
            Instr::Declare(x, target) => write!(f, "new {x:?} --> {target:?}"),
            Instr::Assign(lhs, rhs, target) => write!(f, "{lhs:?}={rhs:?} --> {target:?}"),
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
            } => todo!(),
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
