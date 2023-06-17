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

use vache_lib::{config::Config, execute, Arena, Context};

/// Automatically detects and runs `.vat` (Vache bundled test) files.
///
/// Note: once Vache implements `assert`s, we may want to switch to `.va` files
/// with `assert`s in them.
#[by_resources("vache/tests/exec/**/*.vat")]
#[test]
#[serial_test::serial(compiled)]
fn exec_va(filename: &str) {
    let path = std::path::Path::new(filename);

    // Remove prefix if we are not in the workspace pwd
    let path = match path.strip_prefix("vache") {
        Ok(path) => path,
        Err(_) => path,
    };

    // Check that the path to the resource is correct
    assert!(
        path.exists(),
        "Path of resource {} does not exist",
        path.display()
    );

    let arena = Arena::new();

    // Read the input
    let cur_dir = std::env::current_dir().expect("Current dir not found");
    let input: &str =
        arena.alloc(std::fs::read_to_string(path).unwrap_or_else(|_| {
            panic!("Failed to open file `{filename}` in {}", cur_dir.display())
        }));
    let (input, expected) = input
        .rsplit_once("################################\n")
        .expect("not a valid .vat file, missing the ################################\\n delimiter");

    // Define the config/context of the compiler
    let config = Config {
        input,
        filename: Some(filename),
    };
    let mut context = Context::new(config, &arena);

    // Compile and run the program.
    let program = parse_file(&mut context).expect("Compilation failed");
    let res = execute(&mut context, program, "test-binary", &cur_dir).expect("execution error");

    // Finally, check the result
    assert_eq!(res, expected);
}
