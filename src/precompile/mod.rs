pub mod fun;
pub mod program;
pub mod stmt;

use std::collections::HashSet;

pub use fun::Fun;
use priority_queue::PriorityQueue;
pub use program::Program;
pub use stmt::Stmt;

use crate::mir;
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
        body: precompile_cfg(f.body, f.entry_l, f.ret_l),
    }
}

/// Pre-compiles a CFG.
fn precompile_cfg(cfg: mir::Cfg, entry_l: mir::CfgLabel, exit_l: mir::CfgLabel) -> Vec<Stmt> {
    let mut visited = HashSet::new();
    let mut queue = PriorityQueue::new();
    let scope = cfg[&entry_l].scope;
    queue.push(&entry_l, scope);
    while let Some((label, _)) = queue.pop() {
        println!("Order {label:?}");
        let neighbors = cfg.neighbors(label);
        for neighbor in neighbors {
            if visited.insert(neighbor) {
                let scope = cfg[neighbor].scope;
                queue.push(neighbor, scope);
            }
        }
        let instr = &cfg[label];
    }
    todo!()
}
