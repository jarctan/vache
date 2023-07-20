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

use std::default::default;

use unindent::Unindent;
use vache_lib::{
    borrow_check, check_all, config::Config, execute, farm_modes, interpret, mir, mir::Mode,
    typecheck, Arena, Context,
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

    // Define the config/context of the compiler
    let config = Config {
        input,
        filename: Some(filename),
        ..default()
    };
    let mut context = Context::new(config, &arena);

    let res: Result<_, ::vache_lib::reporter::Diagnostics> = try {
        // Parse and type/borrow check
        let program = parse_file(&mut context)?;

        // Run
        let res =
            execute(&mut context, program, "test-binary", &cur_dir).context("execution error")?;

        // Finally, check the result
        ensure!(
            res == expected,
            format!(
                "output mismatch\nexpected:\n{expected}\nfound:\n{res}\nsee `{filename}` for details"
            )
        );
    };

    if let Err(diagnostics) = res {
        diagnostics.display()?;
        ::anyhow::bail!("Compile errors");
    }
}

/// Automatically detects and runs `.vat` (Vache bundled test) files.
///
/// Note: once Vache implements `assert`s, we may want to switch to `.va` files
/// with `assert`s in them.
#[by_resources("vache/tests/exec/**/*.vat")]
#[test]
fn check_modes_va(filename: &str) {
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
    // Skip the expected section (that should nonetheless exist)
    parts
        .next()
        .context("not a valid .vat file, missing the expected part")?;
    let borrows = parts.next(); // Borrow section is optional

    // Define the config/context of the compiler
    let config = Config {
        input,
        filename: Some(filename),
        ..default()
    };
    let mut context = Context::new(config, &arena);

    let res: Result<_, ::vache_lib::reporter::Diagnostics> = try {
        // Parse and type/borrow check
        let program = parse_file(&mut context)?;
        let program = check_all(&mut context, program)?;

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

                // Match the `line1:col1:line2:col2:mode` pattern
                let mut els = borrow.split(':');
                let wrong_format_error = "Wrong format: expected `line1:col1:line2:col2:mode`";
                let (line1, col1, line2, col2, expected) = (
                    els.next().context(wrong_format_error)?,
                    els.next().context(wrong_format_error)?,
                    els.next().context(wrong_format_error)?,
                    els.next().context(wrong_format_error)?,
                    els.next().context(wrong_format_error)?,
                );

                // Parse column and lines as integers
                // And parse the mode string
                let line1 = line1
                    .parse()
                    .context("Wrong format: line1 is not a number")?;
                let col1 = col1.parse().context("Wrong format: col1 is not a number")?;
                let line2 = line2
                    .parse()
                    .context("Wrong format: col2 is not a number")?;
                let col2 = col2.parse().context("Wrong format: col2 is not a number")?;
                let expected = match expected {
                    "Moved" => Mode::Moved,
                    "Borrowed" => Mode::Borrowed,
                    "Cloned" => Mode::Cloned,
                    _ => {
                        panic!("Wrong format: expected Moved, Borrowed or Cloned, found {expected}")
                    }
                };

                // Fetch the actual mode for that line:col
                let found = modes.get(&LineColSpan {
                    start: LineCol { line: line1, col: col1},
                end:LineCol { line: line2, col: col2}}).with_context(|| {
                    format!("
                    Position {line1}:{col1}:{line2}:{col2} do not correspond to any element with referencing mode in the code.
                    Make sure to indicate the _start_ position of the element to inspect")
                    .unindent()
                })?;

                ensure!(
                    &expected == found,
                    "expected {expected:?}, found {found:?} at {line1}:{col1}:{line2}:{col2}\nsee `{filename}` for details"
                );
            }
        }
    };

    if let Err(diagnostics) = res {
        diagnostics.display()?;
        ::anyhow::bail!("Compile errors");
    }
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
        ..default()
    };
    let mut context = Context::new(config, &arena);

    let res: Result<_, ::vache_lib::reporter::Diagnostics> = try {
        // Parse and type/borrow check
        let program = parse_file(&mut context)?;
        let mut checked = typecheck(&mut context, program)?;
        let mir = borrow_check(&mut context, mir(&mut checked)?)?;

        // Interpret
        let res = interpret(mir).context("execution error")?;

        ensure!(
            res == expected,
            "output mismatch\nexpected:\n{expected}\nfound:\n{res}\nsee `{filename}` for details"
        );
    };
    if let Err(diagnostics) = res {
        diagnostics.display()?;
        ::anyhow::bail!("Compile errors");
    }
}
