pub mod fun;
pub mod program;
pub mod stmt;

use std::{cmp::Ordering, iter::Peekable};

pub use fun::Fun;
pub use program::Program;
pub use stmt::Stmt;

use crate::mir::{self, graph::CfsSelf, Branch, InstrKind};
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
    let body = precompile_instr(
        Stratum::static_stm(),
        &mut cfg.into_cfs(entry_l).peekable(),
        false,
    );
    body
}

/// Pre-compiles instructions in the CFG.
fn precompile_instr(scope: Stratum, iter: &mut Peekable<CfsSelf>, in_loop: bool) -> Vec<Stmt> {
    let mut body = vec![];
    let branch = iter.peek().map(|(b, _, _)| b).cloned().unwrap_or_default();

    while let Some((_, is_loop, i)) = iter.peek() {
        let is_loop = *is_loop;
        match scope.cmp(&i.scope) {
            Ordering::Less => body.push({
                let stmts = precompile_instr(unsafe { scope.incr() }, iter, is_loop);
                if is_loop {
                    Stmt::Loop(stmts)
                } else {
                    Stmt::Block(stmts)
                }
            }),
            Ordering::Greater => {
                assert!(!is_loop);
                if in_loop {
                    body.push(Stmt::Break);
                }
                return body;
            }
            Ordering::Equal => {
                let (_, _, i) = iter.next().unwrap();
                match i.kind {
                    InstrKind::Noop => (),
                    InstrKind::Declare(v) => body.push(Stmt::Declare(v)),
                    InstrKind::Assign(lhs, rhs) => body.push(Stmt::Assign(lhs, rhs)),
                    InstrKind::Call {
                        name,
                        args,
                        destination,
                    } => body.push(Stmt::Call {
                        name,
                        args,
                        destination,
                    }),
                    InstrKind::Branch(v) => {
                        let iftrue = precompile_instr(scope, iter, in_loop);
                        let iffalse = precompile_instr(scope, iter, in_loop);
                        body.push(Stmt::If(v, iftrue, iffalse));
                    }
                }
            }
        }
    }
    body
}
