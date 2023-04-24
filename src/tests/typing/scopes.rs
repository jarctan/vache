use super::*;

/// We assign to a variable `e` another variable `d` which is already out of scope.
#[test]
#[should_panic]
fn wrong_nested_scopes() {
    check(
        vec![Fun {
            name: "wrong_scopes".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                ExprS(block(stmts(vec![Declare(vardef("d", IntT), int(2))]))),
                Declare(vardef("e", IntT), var("d")),
            ]),
        }],
        true,
    );
}

#[test]
fn nested_scopes() {
    check(
        vec![Fun {
            name: "nested_scopes".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: Block {
                stmts: vec![Declare(vardef("e", IntT), int(2))],
                ret: block(stmts(vec![Declare(vardef("d", IntT), var("e"))])),
            },
        }],
        true,
    );
}
