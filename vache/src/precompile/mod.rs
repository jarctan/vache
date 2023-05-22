//! Translates the CFG back into some IR with loops and blocks.
//!
//! This is useful to translate back into Rust source code which has no support
//! for goto's in their syntax.

pub mod fun;
pub mod program;
pub mod stmt;

pub use fun::Fun;
pub use program::Program;
pub use stmt::Stmt;

use crate::mir::{self};
pub use crate::mir::{Mode, Place, RValue, Stratum, Struct, Ty, Var, VarDef, VarMode};

/// Pre-compiles a program.
pub fn precompile(program: mir::Program) -> Program {
    Program {
        funs: program
            .funs
            .into_iter()
            .map(|(name, f)| (name, precompile_fun(f)))
            .collect(),
        structs: program.structs,
    }
}

/// Pre-compiles a function.
fn precompile_fun(f: mir::Fun) -> Fun {
    Fun {
        name: f.name,
        params: f.params,
        ret_v: f.ret_v,
        body: precompile_cfg(f.body, f.entry_l),
    }
}

/// Pre-compiles a CFG.
fn precompile_cfg(cfg: mir::Cfg, entry_l: mir::CfgLabel) -> Vec<Stmt> {
    let dominators = cfg.immediate_dominators(&entry_l);
    println!("Dominators: {:?}", dominators);
    for _ in dominators {}
    todo!()
}
