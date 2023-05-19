use super::*;

#[vache_test("10\n9\n8\n7\n6\n5\n4\n")]
fn while_loop() -> Program {
    examples::while_loop().into()
}

#[vache_test("10\n9\n8\n7\n6\n5\n4\n4\n3\n2\n1\n0\n")]
fn while_loop2() -> Program {
    examples::while_loop2().into()
}
