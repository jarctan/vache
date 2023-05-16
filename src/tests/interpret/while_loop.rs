use super::*;

#[test]
fn while_loop() {
    test(examples::while_loop(), "10\n9\n8\n7\n6\n5\n4\n");
}

#[test]
fn while_loop2() {
    test(
        examples::while_loop2(),
        "10\n9\n8\n7\n6\n5\n4\n4\n3\n2\n1\n0\n",
    );
}
