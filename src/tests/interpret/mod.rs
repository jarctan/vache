use super::*;

mod arrays;
mod borrows;
mod change_field;
mod custom_addition;
mod hello_world;
mod id_fn;
mod is_even;
mod matrices;
mod multiple_refs;
mod out_of_scope;
mod structures;
mod while_loop;

fn test(p: impl Into<Program>, output: impl AsRef<str>) {
    let mir = crate::mir(crate::check(p.into()));
    println!("MIR: {:#?}", mir);
    assert_eq!(crate::interpret(mir), output.as_ref(), "Output mismatch");
}