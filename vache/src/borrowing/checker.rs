//! Taking a program and computing the borrow-checking analysis on it.
//!
//! To use it:
//! * Instantiate `BorrowChecker::new()`
//! * then `borrow_checker.check(&your_program)`

use std::collections::HashMap;

use anyhow::Result;

use super::fun_flow::builtin_flows;
use super::{liveness, FunFlow};
use crate::mir::{Fun, Program};
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
    ) -> Result<Program<'mir, 'ctx>> {
        self.visit_program(ctx, p)
    }

    /// Borrow-checks a function.
    ///
    /// Returns the new function signature.
    fn visit_fun<'ctx>(
        &mut self,
        f: &mut Fun<'_, 'ctx>,
        ctx: &mut Context<'ctx>,
        fun_flow: &HashMap<&'ctx str, FunFlow>,
    ) -> Result<FunFlow> {
        let res = liveness(f, fun_flow, ctx)?;
        // println!("Flow for {} is {res:?}", f.name);
        // f.body.print_image(f.name)?;
        Ok(res)
    }

    /// Borrow-checks a program.
    fn visit_program<'mir, 'ctx>(
        &mut self,
        ctx: &mut Context<'ctx>,
        mut p: Program<'mir, 'ctx>,
    ) -> Result<Program<'mir, 'ctx>> {
        // Bootstrap function flow with one for each function.
        // The flow is empty for all functions except the builtins.
        let mut fun_flow = builtin_flows();
        for name in p.funs.keys() {
            fun_flow.insert(name, FunFlow::default());
        }

        // Compute the fixpoint
        // Iterate until all function signatures are stabilized
        let mut updated = true;
        while updated {
            updated = false;
            // Restart with at least the builtin flows
            let mut new_fun_flow = builtin_flows();

            // For each function, add its flow to the new map
            for (&name, f) in &mut p.funs {
                let new_flow = self.visit_fun(f, ctx, &fun_flow)?;
                // If the flow is updated, we'll need to do a new iteration of the `while` loop
                if new_flow != fun_flow[name] {
                    updated = true;
                }
                // In any case, push the new flow to the new map
                new_fun_flow.insert(name, new_flow);
            }

            // Finally, update the overall flow with the newly computed one
            fun_flow = new_fun_flow;
        }

        if ctx.reporter.has_errors() {
            Err(anyhow!("found borrow-check errors"))
        } else {
            Ok(p)
        }
    }
}
