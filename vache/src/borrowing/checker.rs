//! Taking a program and computing the borrow-checking analysis on it.
//!
//! To use it:
//! * Instantiate `BorrowChecker::new()`
//! * then `borrow_checker.check(&your_program)`

use std::collections::HashMap;
use std::default::default;

use super::liveness;
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
    ) -> Result<Fun<'mir, 'ctx>, Diagnostics<'ctx>> {
        f.body = liveness(f.body, f.entry_l, f.ret_l, &f.strata)?;
        Ok(f)
    }

    /// Borrow-checks a program.
    fn visit_program<'mir, 'ctx>(
        &mut self,
        ctx: &mut Context<'ctx>,
        p: Program<'mir, 'ctx>,
    ) -> Result<Program<'mir, 'ctx>, Diagnostics<'ctx>> {
        let mut funs: HashMap<&str, Fun> = default();
        for (name, f) in p.funs {
            funs.insert(name, self.visit_fun(f)?);
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
