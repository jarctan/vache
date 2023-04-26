use super::*;

mod custom_addition;
mod hello_world;
mod is_even;
mod multiple_refs;
mod out_of_scope;
mod structures;

fn test(p: impl Into<Program>, output: impl AsRef<str>) {
    assert_eq!(
        crate::interp(crate::check(p.into())),
        output.as_ref(),
        "Output mismatch"
    );
}
