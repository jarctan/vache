//! Parsing statements, and defining their representation in the AST.

use pest::iterators::Pair;

use super::{Block, Expr, Place, VarDef};
use super::{Context, Parsable};
use crate::grammar::*;
use crate::utils::boxed;

/// A statement.
#[derive(Debug, Clone)]
pub enum Stmt<'ctx> {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    Declare(VarDef<'ctx>, Expr<'ctx>),
    /// An assignment.
    Assign(Place<'ctx>, Expr<'ctx>),
    /// An expression, whose final value is discarded.
    ExprS(Expr<'ctx>),
    /// A while statement.
    While {
        /// Condition.
        cond: Expr<'ctx>,
        /// While body.
        body: Block<'ctx>,
    },
}

impl<'ctx> Stmt<'ctx> {
    /// Sees this statement as a declaration.
    ///
    /// Returns: `(declare, expr)`.
    ///
    /// # Errors
    /// Returns `None` if the statement is not a declaration.
    pub fn as_declare(&self) -> Option<(&VarDef, &Expr)> {
        if let Stmt::Declare(vardef, expr) = self {
            Some((vardef, expr))
        } else {
            None
        }
    }

    /// Sees this statement as a assignment.
    ///
    /// Returns: `(lhs, expr)`.
    ///
    /// # Errors
    /// Returns `None` if the statement is not a assignment.
    pub fn as_assign(&self) -> Option<(&Place, &Expr)> {
        if let Stmt::Assign(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    /// Sees this statement as an expression.
    ///
    /// Returns: `(lhs, expr)`.
    ///
    /// # Errors
    /// Returns `None` if the statement is not an expression.
    pub fn as_expr(&self) -> Option<&Expr> {
        if let Stmt::ExprS(expr) = self {
            Some(expr)
        } else {
            None
        }
    }

    /// Sees this statement as a while loop.
    ///
    /// Returns: `(cond, body)`.
    ///
    /// # Errors
    /// Returns `None` if the statement is not a while loop.
    pub fn as_while_loop(&self) -> Option<(&Expr, &Block)> {
        if let Stmt::While { cond, body } = self {
            Some((cond, body))
        } else {
            None
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Stmt<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        assert!(matches!(pair.as_rule(), Rule::stmt));
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::declare => {
                let mut pairs = pair.into_inner();
                let vardef = ctx.parse(pairs.next().unwrap());
                let rhs = ctx.parse(pairs.next().unwrap());
                Stmt::Declare(vardef, rhs)
            }
            Rule::assign => {
                let mut pairs = pair.into_inner();
                let lhs = ctx.parse(pairs.next().unwrap());
                let rhs = ctx.parse(pairs.next().unwrap());
                Stmt::Assign(lhs, rhs)
            }
            Rule::expr => {
                let expr = ctx.parse(pair);
                Stmt::ExprS(expr)
            }
            Rule::while_loop => {
                let mut pairs = pair.into_inner();
                println!("{:?}", pairs);
                let cond = ctx.parse(pairs.next().unwrap());
                let body = ctx.parse(pairs.next().unwrap());
                Stmt::While { cond, body }
            }
            rule => panic!("parser internal error: expected statement, found {rule:?}"),
        }
    }
}

/// Shortcut to print several expressions in our program.
pub fn print<'ctx>(stmts: impl IntoIterator<Item = Expr<'ctx>>) -> Stmt<'ctx> {
    Stmt::ExprS(super::expr::call("print", stmts))
}

/// Shortcut to make a call.
pub fn call_stmt<'ctx>(name: &'ctx str, stmts: impl IntoIterator<Item = Expr<'ctx>>) -> Stmt<'ctx> {
    Stmt::ExprS(super::expr::call(name, stmts))
}

/// Shortcut for a block statement.
pub fn block_stmt<'ctx>(b: impl Into<Block<'ctx>>) -> Stmt<'ctx> {
    Stmt::ExprS(Expr::BlockE(boxed(b.into())))
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use Expr::*;

    use super::super::Ty;
    use super::*;

    #[parses("let x: int = 4" as stmt)]
    #[test]
    fn declaration(stmt: Stmt) {
        let (lhs, rhs) = stmt.as_declare().unwrap();
        assert_eq!(lhs.name, "x");
        assert_eq!(lhs.ty, Ty::IntT);
        assert_eq!(rhs.as_integer().unwrap(), &BigInt::from(4));
    }

    #[parses("y = 5" as stmt)]
    #[test]
    fn assignment(stmt: Stmt) {
        let (lhs, rhs) = stmt.as_assign().unwrap();
        assert_eq!(lhs, "y");
        assert_eq!(rhs.as_integer().unwrap(), &BigInt::from(5));
    }

    #[parses("\"test\"" as stmt)]
    #[test]
    fn expression(stmt: Stmt) {
        let expr = stmt.as_expr().unwrap();
        assert_eq!(expr.as_string().unwrap(), "test");
    }

    #[parses("while x { x = x; print(x); }" as stmt)]
    #[test]
    fn while_loop(stmt: Stmt) {
        let (cond, block) = stmt.as_while_loop().unwrap();
        assert!(cond.as_var().unwrap() == "x");
        assert!(matches!(
            &block.stmts[..],
            [Stmt::Assign(..), Stmt::ExprS(CallE { .. })]
        ));
    }
}
