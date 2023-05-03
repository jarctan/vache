use super::*;

mod borrows;
mod change_field;
mod custom_addition;
mod hello_world;
mod id_fn;
mod is_even;
mod multiple_refs;
mod out_of_scope;
mod structures;
mod while_loop;

fn test(p: impl Into<Program>, output: impl AsRef<str>) {
    assert_eq!(
        crate::interp(crate::mir(crate::check(p.into()))),
        output.as_ref(),
        "Output mismatch"
    );
}
