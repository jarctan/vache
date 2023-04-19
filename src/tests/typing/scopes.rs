use super::*;

/// Here, we introduce two variables in two nested blocks, and try to assign the
/// short-lived variable to the long-lived one, which must fail.
#[test]
#[should_panic]
fn wrong_nested_scopes() {
    check(
        |_static| {
            let f = Fun {
                name: "wrong_scopes".to_string(),
                quantifiers: vec![],
                params: vec![],
                ret_ty: ret_ty(UnitT, _static),
                body: stmts(|_| {
                    vec![
                        ExprS(block(stmts(|s| {
                            vec![Declare(vardef("d", s, IntT), int(2))]
                        }))),
                        Declare(vardef("e", _static, IntT), var("d")),
                    ]
                }),
            };
            vec![f]
        },
        true,
    );
}

/// Here, same as `wrong_nested_scopes`, but we introduce a statically-scoped variable.
/// Therefore, this is fine: we always assign an expression to a variable of a lower scope.
#[test]
fn assigned_in_static() {
    check(
        |_static| {
            let f = Fun {
                name: "wrong_scopes".to_string(),
                quantifiers: vec![],
                params: vec![],
                ret_ty: ret_ty(UnitT, _static),
                body: stmts(|s| {
                    vec![
                        ExprS(block(stmts(|_| {
                            vec![Declare(vardef("d", _static, IntT), int(2))]
                        }))),
                        Declare(vardef("e", s, IntT), var("d")),
                    ]
                }),
            };
            vec![f]
        },
        true,
    );
}

#[test]
fn nested_scopes() {
    check(
        |_static| {
            let f = Fun {
                name: "nested_scopes".to_string(),
                quantifiers: vec![],
                params: vec![],
                ret_ty: ret_ty(UnitT, _static),
                body: Block {
                    stratum: Stratum::new_concrete(),
                    stmts: vec![Declare(vardef("e", _static, IntT), int(2))],
                    ret: block(stmts(|s| vec![Declare(vardef("d", s, IntT), var("e"))])),
                },
            };
            vec![f]
        },
        true,
    );
}
