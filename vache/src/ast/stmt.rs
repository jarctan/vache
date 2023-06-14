//! Parsing statements, and defining their representation in the AST.

use std::default::default;

use pest::iterators::Pair;
use ExprKind::*;

use super::expr::parse_if_then_else;
use super::{Block, Expr, ExprKind, Place, Span, VarDef, VarUse};
use super::{Context, Parsable};
use crate::grammar::*;
use crate::utils::boxed;

/// A located statement in the code.
#[derive(Debug, Clone)]
pub struct Stmt<'ctx> {
    /// Expression kind.
    pub kind: StmtKind<'ctx>,
    /// Codespan.
    pub span: Span,
}

impl<'ctx> From<StmtKind<'ctx>> for Stmt<'ctx> {
    fn from(kind: StmtKind<'ctx>) -> Self {
        Self {
            kind,
            span: default(),
        }
    }
}

/// Statement kind.
#[derive(Debug, Clone)]
pub enum StmtKind<'ctx> {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    DeclareS(VarDef<'ctx>, Expr<'ctx>),
    /// An assignment.
    AssignS(Place<'ctx>, Expr<'ctx>),
    /// An expression, whose final value is discarded.
    ExprS(Expr<'ctx>),
    /// A while loop.
    WhileS {
        /// Condition.
        cond: Expr<'ctx>,
        /// While body.
        body: Block<'ctx>,
    },
    /// A for loop.
    ForS {
        /// Item used within the loop.
        ///
        /// TODO: change to `VarDef` when type inference is implemented.
        item: VarUse<'ctx>,
        /// Element being iterated over.
        iter: Expr<'ctx>,
        /// For loop body.
        body: Block<'ctx>,
    },
}

use StmtKind::*;

impl<'ctx> Stmt<'ctx> {
    /// Sees this statement as a declaration.
    ///
    /// Returns: `(declare, expr)`.
    ///
    /// # Errors
    /// Returns `None` if the statement is not a declaration.
    pub fn as_declare(&self) -> Option<(&VarDef, &Expr)> {
        if let DeclareS(vardef, expr) = &self.kind {
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
        if let AssignS(lhs, rhs) = &self.kind {
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
        if let ExprS(expr) = &self.kind {
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
        if let WhileS { cond, body } = &self.kind {
            Some((cond, body))
        } else {
            None
        }
    }

    /// Sees this statement as a `for` loop.
    ///
    /// Returns: `(item, iter, body)`.
    ///
    /// # Errors
    /// Returns `None` if the statement is not a `for` loop.
    pub fn as_for_lop(&self) -> Option<(&VarUse, &Expr, &Block)> {
        if let ForS { item, iter, body } = &self.kind {
            Some((item, iter, body))
        } else {
            None
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Stmt<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        assert!(matches!(pair.as_rule(), Rule::stmt));
        let pair = pair.into_inner().next().unwrap();
        let span = Span::from(pair.as_span());
        let kind = match pair.as_rule() {
            Rule::declare => {
                let mut pairs = pair.into_inner();
                let vardef = ctx.parse(pairs.next().unwrap());
                let rhs = ctx.parse(pairs.next().unwrap());
                DeclareS(vardef, rhs)
            }
            Rule::assign => {
                let mut pairs = pair.into_inner();
                let lhs = ctx.parse(pairs.next().unwrap());
                let rhs = ctx.parse(pairs.next().unwrap());
                AssignS(lhs, rhs)
            }
            Rule::expr => {
                let expr = ctx.parse(pair);
                ExprS(expr)
            }
            Rule::while_loop => {
                let mut pairs = pair.into_inner();
                let cond = ctx.parse(pairs.next().unwrap());
                let body = ctx.parse(pairs.next().unwrap());
                WhileS { cond, body }
            }
            Rule::for_loop => {
                let mut pairs = pair.into_inner();
                let item = ctx.parse(pairs.next().unwrap());
                let iter = ctx.parse(pairs.next().unwrap());
                let body = ctx.parse(pairs.next().unwrap());
                ForS { item, iter, body }
            }
            Rule::if_then => ExprS(parse_if_then_else(ctx, pair)),
            rule => panic!("parser internal error: expected statement, found {rule:?}"),
        };
        Stmt { span, kind }
    }
}

/// Shortcut to declare a variable in our program.
pub fn declare<'ctx>(lhs: impl Into<VarDef<'ctx>>, rhs: impl Into<Expr<'ctx>>) -> Stmt<'ctx> {
    DeclareS(lhs.into(), rhs.into()).into()
}

/// Shortcut to assign a variable in our program.
pub fn assign<'ctx>(lhs: impl Into<Place<'ctx>>, rhs: impl Into<Expr<'ctx>>) -> Stmt<'ctx> {
    AssignS(lhs.into(), rhs.into()).into()
}

/// Shortcut to print several expressions in our program.
pub fn print<'ctx>(stmts: impl IntoIterator<Item = Expr<'ctx>>) -> Stmt<'ctx> {
    ExprS(super::expr::call("print", stmts)).into()
}

/// Shortcut to make a call.
pub fn call_stmt<'ctx>(name: &'ctx str, stmts: impl IntoIterator<Item = Expr<'ctx>>) -> Stmt<'ctx> {
    ExprS(super::expr::call(name, stmts)).into()
}

/// Shortcut for a block statement.
pub fn block_stmt<'ctx>(b: impl Into<Block<'ctx>>) -> Stmt<'ctx> {
    ExprS(BlockE(boxed(b.into())).into()).into()
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;

    use super::super::Ty;
    use super::*;

    #[parses("var x: int = 4;" as stmt)]
    #[test]
    fn declaration(stmt: Stmt) {
        let (lhs, rhs) = stmt.as_declare().context("is not a declare")?;
        assert_eq!(lhs.var, "x");
        assert_eq!(lhs.ty, Ty::IntT);
        assert_eq!(rhs.as_integer().unwrap(), &BigInt::from(4));
    }

    #[parses("y = 5;" as stmt)]
    #[test]
    fn assignment(stmt: Stmt) {
        let (lhs, rhs) = stmt.as_assign().context("is not a declare")?;
        assert_eq!(lhs, "y");
        assert_eq!(rhs.as_integer().unwrap(), &BigInt::from(5));
    }

    #[parses("\"test\";" as stmt)]
    #[test]
    fn expression(stmt: Stmt) {
        let expr = stmt.as_expr().context("is not a declare")?;
        assert_eq!(expr.as_string().unwrap(), "test");
    }

    #[parses("while x { x = x; print(x); }" as stmt)]
    #[test]
    fn while_loop(stmt: Stmt) {
        let (cond, block) = stmt.as_while_loop().unwrap();
        assert!(cond.as_var().unwrap() == "x");
        assert!(matches!(block.stmts[0].kind, AssignS(..)));
        assert!(matches!(
            block.stmts[1]
                .as_expr()
                .expect("2nd statement should be an expression")
                .kind,
            CallE { .. }
        ));
    }
}
