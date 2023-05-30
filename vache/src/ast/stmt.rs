//! Parsing statements, and defining their representation in the AST.

use pest::iterators::Pair;

use super::{Block, Expr, Place, VarDef};
use super::{Context, Parsable};
use crate::grammar::*;
use crate::utils::boxed;

/// A statement.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    Declare(VarDef, Expr),
    /// An assignment.
    Assign(Place, Expr),
    /// An expression, whose final value is discarded.
    ExprS(Expr),
    /// A while statement.
    While {
        /// Condition.
        cond: Expr,
        /// While body.
        body: Block,
    },
}

impl Stmt {
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

impl Parsable<Pair<'_, Rule>> for Stmt {
    fn parse(pair: Pair<Rule>, ctx: &mut Context) -> Self {
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
pub fn print(stmts: impl IntoIterator<Item = Expr>) -> Stmt {
    Stmt::ExprS(super::expr::call("print", stmts))
}

/// Shortcut to make a call.
pub fn call_stmt(name: impl ToString, stmts: impl IntoIterator<Item = Expr>) -> Stmt {
    Stmt::ExprS(super::expr::call(name, stmts))
}

/// Shortcut for a block statement.
pub fn block_stmt(b: impl Into<Block>) -> Stmt {
    Stmt::ExprS(Expr::BlockE(boxed(b.into())))
}

impl PartialEq for Stmt {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl Eq for Stmt {}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use pest::Parser;
    use Expr::*;

    use super::super::Ty;
    use super::*;
    use crate::grammar::Grammar;

    #[test]
    fn declaration() {
        let input = "let x: int = 4";
        let mut parsed = Grammar::parse(Rule::stmt, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let stmt: Stmt = ctx.parse(pair);
        eprintln!("{stmt:?}");
        let (lhs, rhs) = stmt.as_declare().unwrap();
        assert_eq!(lhs.name, "x");
        assert_eq!(lhs.ty, Ty::IntT);
        assert_eq!(rhs.as_integer().unwrap(), &BigInt::from(4));
    }

    #[test]
    fn assignment() {
        let input = "y = 5";
        let mut parsed = Grammar::parse(Rule::stmt, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let stmt: Stmt = ctx.parse(pair);
        eprintln!("{stmt:?}");
        let (lhs, rhs) = stmt.as_assign().unwrap();
        assert_eq!(lhs, "y");
        assert_eq!(rhs.as_integer().unwrap(), &BigInt::from(5));
    }

    #[test]
    fn expression() {
        let input = "\"test\"";
        let mut parsed = Grammar::parse(Rule::stmt, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let stmt: Stmt = ctx.parse(pair);
        eprintln!("{stmt:?}");
        let expr = stmt.as_expr().unwrap();
        assert_eq!(expr.as_string().unwrap(), "test");
    }

    #[test]
    fn while_loop() {
        let input = "while x { x = x; print(x); }";
        let mut parsed = Grammar::parse(Rule::stmt, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Stmt = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (cond, block) = expr.as_while_loop().unwrap();
        assert!(cond.as_var().unwrap() == "x");
        assert!(matches!(
            &block.stmts[..],
            [Stmt::Assign(..), Stmt::ExprS(CallE { .. })]
        ));
    }
}
