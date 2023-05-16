use super::*;

#[test]
fn change_field() {
    test(
        examples::change_field(),
        "dupont doe\n", // Is it what we expect?
    );
}
