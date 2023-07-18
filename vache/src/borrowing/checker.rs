//! Taking a program and computing the borrow-checking analysis on it.
//!
//! To use it:
//! * Instantiate `BorrowChecker::new()`
//! * then `borrow_checker.check(&your_program)`

use std::collections::HashMap;
use std::default::default;

use super::fun_flow::builtin_flows;
use super::{liveness, FunFlow};
use crate::mir::{Fun, Program};
use crate::reporter::Diagnostics;
use crate::Context;

/// The borrow-checker.
pub struct BorrowChecker {}

impl BorrowChecker {
    /// Creates a new borrow-checker.
    pub fn new() -> Self {
        Self {}
    }

    /// Borrow-checks a given program.
    pub fn check<'mir, 'ctx>(
        &mut self,
        ctx: &mut Context<'ctx>,
        p: Program<'mir, 'ctx>,
    ) -> Result<Program<'mir, 'ctx>, Diagnostics<'ctx>> {
        self.visit_program(ctx, p)
    }

    /// Borrow-checks a function.
    fn visit_fun<'mir, 'ctx>(
        &mut self,
        mut f: Fun<'mir, 'ctx>,
        ctx: &mut Context<'ctx>,
        fun_flow: &mut HashMap<&'ctx str, FunFlow>,
    ) -> Result<Fun<'mir, 'ctx>, Diagnostics<'ctx>> {
        f.body = liveness(
            f.body,
            f.entry_l,
            f.ret_l,
            fun_flow,
            &f.strata,
            &mut ctx.reporter,
        )?;
        Ok(f)
    }

    /// Borrow-checks a program.
    fn visit_program<'mir, 'ctx>(
        &mut self,
        ctx: &mut Context<'ctx>,
        p: Program<'mir, 'ctx>,
    ) -> Result<Program<'mir, 'ctx>, Diagnostics<'ctx>> {
        let mut fun_flow = builtin_flows();

        // Bootstrap function flow with one for each function.
        for (name, f) in &p.funs {
            // Let's use the maximal flow here since we don't do any fixpoint
            // The result depends on all the arguments, and the arguments depends on all
            // arguments except themselves.
            let nb_params = f.params.len();
            let all_deps = || 0..nb_params;
            fun_flow.insert(
                name,
                FunFlow::new(
                    f.params
                        .iter()
                        .enumerate()
                        .filter(|(_, r)| r.byref)
                        .map(|(i, _)| {
                            // return all dependencies but the current i
                            (i, all_deps().filter(move |&j| j != i))
                        }),
                    all_deps(),
                ),
            );
        }
        let mut funs: HashMap<&str, Fun> = default();

        for (name, f) in p.funs {
            funs.insert(name, self.visit_fun(f, ctx, &mut fun_flow)?);
        }

        if ctx.reporter.has_errors() {
            Err(ctx.reporter.flush())
        } else {
            Ok(Program {
                funs,
                structs: p.structs,
                enums: p.enums,
            })
        }
    }
}
