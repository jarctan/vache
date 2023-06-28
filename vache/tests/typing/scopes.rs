use super::*;

/// We assign to a variable `e` another variable `d` which is already out of
/// scope.
#[should_fail(UNKNOWN_VAR_ERROR)]
#[test]
fn wrong_nested_scopes() -> Program {
    vec![Fun {
        name: "main",
        body: stmts(vec![
            ExprS(block(stmts(vec![declare(vardef("d", IntT), int(2))]))).into(),
            declare(vardef("e", IntT), var("d")),
        ]),
        ..default()
    }]
    .into()
}

#[test]
fn nested_scopes() -> Result<()> {
    test(vec![Fun {
        name: "main",
        body: Block {
            stmts: vec![declare(vardef("e", IntT), int(2))],
            ret: block(stmts(vec![declare(vardef("d", IntT), var("e"))])),
            span: default(),
        },
        ..default()
    }])
}
