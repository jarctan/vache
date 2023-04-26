use super::*;

mod custom_addition;
mod hello_world;
mod is_even;
mod multiple_refs;
mod out_of_scope;

fn test(p: Program, output: impl AsRef<str>) {
    assert_eq!(
        crate::interp(crate::check(p)),
        output.as_ref(),
        "Output mismatch"
    );
}
