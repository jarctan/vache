use super::*;

#[test]
#[should_panic]
fn wrong_nested_scopes() {
    check(|_static| {
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
    });
}

#[test]
fn nested_scopes() {
    check(|_static| {
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
    });
}
