//! Toy Stratum language compiler.

use vache_lib::ast::block::*;
use vache_lib::ast::expr::*;
use vache_lib::ast::ty::*;
use vache_lib::ast::var::*;
use vache_lib::ast::*;
use vache_lib::{check, interp};
use Expr::*;
use Stmt::*;
use Ty::*;

fn main() {
    interp(check({
        let f1 = Fun {
            name: "is_even".to_string(),
            params: vec![vardef("n", IntT)],
            ret_ty: BoolT,
            body: Block {
                stmts: vec![
                    Declare(vardef("x", IntT), int(2)),
                    Declare(vardef("res", IntT), binop(var("n"), "%", var("x"))),
                ],
                ret: binop(var("res"), "==", int(0)),
            },
        };

        let f2 = Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![ExprS(call(
                "print",
                vec![call("is_even", vec![int(10)])],
            ))]),
        };

        vec![f1, f2]
    }));
}
