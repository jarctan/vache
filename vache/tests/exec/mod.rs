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
mod structures;
mod while_loop;

use unindent::Unindent;
use vache_lib::{
    borrow_check, check_all, config::Config, farm_modes, interpret, mir, mir::Mode, run, typecheck,
    Arena, Context,
};

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
        arena.alloc(std::fs::read_to_string(path).with_context(|| {
            format!("Failed to open file `{filename}` in {}", cur_dir.display())
        })?);
    let mut parts = input.split("################################\n");
    let input = parts.next().expect("file is empty");
    let expected = parts
        .next()
        .context("not a valid .vat file, missing the expected part")?;
    let borrows = parts.next(); // Borrow section is optional

    // Define the config/context of the compiler
    let config = Config {
        input,
        filename: Some(filename),
    };
    let mut context = Context::new(config, &arena);

    // Parse and type/borrow check
    let program = parse_file(&mut context).context("Compilation failed")?;
    let program = check_all(&mut context, program).context("Compilation error")?;

    // Get the mode for every place
    let modes = farm_modes(&mut context, &program);

    // If we specified a borrow-testing section
    if let Some(borrows) = borrows {
        // One borrow per line, so split lines
        for borrow in borrows
            .split('\n')
            .map(str::trim)
            .filter(|borrow| !borrow.is_empty())
        {
            // Discard comments at the end
            let borrow = borrow
                .split_once("//")
                .map(|(borrow, _comments)| borrow)
                .unwrap_or(borrow)
                .trim(); // <- Trim any whitespace

            // Match the `line:col:mode` pattern
            let mut els = borrow.split(':');
            let (line, col, expected) = (
                els.next()
                    .context("Wrong format: expected `line:col:mode`")?,
                els.next()
                    .context("Wrong format: expected `line:col:mode`")?,
                els.next()
                    .context("Wrong format: expected `line:col:mode`")?,
            );

            // Parse column and lines as integers
            // And parse the mode string
            let line = line.parse().context("Wrong format: line is not a number")?;
            let col = col.parse().context("Wrong format: col is not a number")?;
            let expected = match expected {
                "Moved" => Mode::Moved,
                "Borrowed" => Mode::Borrowed,
                "Cloned" => Mode::Cloned,
                _ => panic!("Wrong format: expected Moved, Borrowed or Cloned, found {expected}"),
            };

            // Fetch the actual mode for that line:col
            let found = modes.get(&(line, col)).with_context(|| {
                format!("
                Position {line}:{col} do not correspond to any element with referencing mode in the code.
                Make sure to indicate the _start_ position of the element to inspect")
                .unindent()
            })?;

            ensure!(
                &expected == found,
                "expected {expected:?}, found {found:?} at {line}:{col}\nsee `{filename}` for details"
            );
        }
    }

    // Run
    let res = run(&mut context, program, "test-binary", &cur_dir).context("execution error")?;

    // Finally, check the result
    ensure!(
        res == expected,
        format!(
            "output mismatch\nexpected:\n{expected}\nfound:\n{res}\nsee `{filename}` for details"
        )
    );
}

/// Automatically detects and interprets `.vat` (Vache bundled test) files.
///
/// Note: once Vache implements `assert`s, we may want to switch to `.va` files
/// with `assert`s in them.
#[by_resources("vache/tests/exec/**/*.vat")]
#[test]
fn interpret_va(filename: &str) {
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
        arena.alloc(std::fs::read_to_string(path).with_context(|| {
            format!("Failed to open file `{filename}` in {}", cur_dir.display())
        })?);
    let mut parts = input.split("################################\n");
    let input = parts.next().expect("file is empty");
    let expected = parts
        .next()
        .context("not a valid .vat file, missing the expected part")?;

    // Define the config/context of the compiler
    let config = Config {
        input,
        filename: Some(filename),
    };
    let mut context = Context::new(config, &arena);

    // Parse and type/borrow check
    let program = parse_file(&mut context).context("Compilation failed")?;

    match typecheck(&mut context, program)? {
        Ok(mut checked) => {
            let mir = borrow_check(mir(&mut checked)?)?;
            eprintln!("MIR: {mir:?}");

            // Interpret
            let res = interpret(mir).context("execution error")?;

            ensure!(
                res == expected,
            "output mismatch\nexpected:\n{expected}\nfound:\n{res}\nsee `{filename}` for details"
            );
        }
        Err(diagnostics) => {
            diagnostics.display()?;
            ::anyhow::bail!("Compile errors");
        }
    }
}
