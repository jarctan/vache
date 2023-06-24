//! Taking a program and computing the borrow-checking analysis on it.
//!
//! To use it:
//! * Instantiate `BorrowChecker::new()`
//! * then `borrow_checker.check(&your_program)`

use std::collections::HashMap;
use std::default::default;

use codespan_reporting::files::SimpleFile;

use super::liveness;
use crate::mir::{Fun, Program};
use crate::reporter::{Diagnostics, Reporter};

/// The borrow-checker.
pub struct BorrowChecker {}

impl BorrowChecker {
    /// Creates a new borrow-checker.
    pub fn new() -> Self {
        Self {}
    }

    /// Borrow-checks a given program.
    pub fn check<'ctx>(&mut self, p: Program<'ctx>) -> Result<Program<'ctx>, Diagnostics<'ctx>> {
        self.visit_program(p)
    }

    /// Borrow-checks a function.
    fn visit_fun<'ctx>(
        &mut self,
        reporter: &mut Reporter<'ctx>,
        mut f: Fun<'ctx>,
    ) -> Result<Fun<'ctx>, Diagnostics<'ctx>> {
        f.body = liveness(f.body, f.entry_l, f.ret_l, &f.strata, reporter)?;
        Ok(f)
    }

    /// Borrow-checks a program.
    fn visit_program<'ctx>(
        &mut self,
        p: Program<'ctx>,
    ) -> Result<Program<'ctx>, Diagnostics<'ctx>> {
        let mut funs: HashMap<&str, Fun> = default();
        let files = p
            .arena
            .alloc(SimpleFile::new("unknown file", "unknown input"));
        let mut reporter = Reporter::new(p.arena, files);
        for (name, f) in p.funs {
            funs.insert(name, self.visit_fun(&mut reporter, f)?);
        }

        if reporter.has_errors() {
            Err(reporter.flush())
        } else {
            Ok(Program {
                arena: p.arena,
                funs,
                structs: p.structs,
                enums: p.enums,
            })
        }
    }
}
