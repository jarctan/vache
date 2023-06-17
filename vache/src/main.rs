//! Toy Vache language compiler.

#![feature(try_blocks)]

use std::process::Command;

use anyhow::{bail, Context as AnyhowContext};
use clap::{Parser, Subcommand};
use unindent::Unindent;
use vache_lib::{
    borrow_check, check_all, compile, config::Config, examples::parse_file, execute, farm_modes,
    mir, typecheck, Arena, Context,
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

/// Vache commands.
#[derive(Subcommand, Debug)]
enum Commands {
    /// Gives the MIR output of a program.
    Mir {
        /// The input file to compile.
        filename: String,
    },
    /// Compiles a program.
    Compile {
        /// The input file to compile.
        filename: String,
    },
    /// Runs a program.
    Run {
        /// The input file to compile.
        filename: String,
    },
    /// Gets the addressing modes of all elements of the program.
    Modes {
        /// The input file to compile.
        filename: String,
    },
}

fn main() -> anyhow::Result<()> {
    // Check that cargo exist, since we need it to compile.
    let cargo_chk = Command::new("cargo").arg("--version").output()?;
    if !cargo_chk.status.success() {
        bail!("rust/cargo binary could not be found on the system.
            You need to install rust + cargo first (https://rustup.rs/)."
            .unindent());
    }

    match Cli::parse().command {
        Commands::Mir { ref filename } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
            };
            let mut context = Context::new(config, &arena);

            let program = parse_file(&mut context).context("Compilation failed")?;
            match typecheck(&mut context, program)? {
                Ok(mut checked) => {
                    let mir = borrow_check(mir(&mut checked)?)?;
                    println!("{:#?}", mir);
                    Ok(())
                }
                Err(diagnostics) => {
                    diagnostics.display()?;
                    bail!("Compile errors found");
                }
            }
        }
        Commands::Compile { ref filename } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
            };
            let mut context = Context::new(config, &arena);

            let program = parse_file(&mut context).context("Compilation failed")?;
            compile(&mut context, program, "binary", &cur_dir)?;
            Ok(())
        }
        Commands::Run { ref filename } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
            };
            let mut context = Context::new(config, &arena);
            let program = parse_file(&mut context).context("Compilation failed")?;
            let res =
                execute(&mut context, program, "binary", &cur_dir).context("execution error")?;
            println!("{}", res);
            Ok(())
        }
        Commands::Modes { ref filename } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
            };
            let mut context = Context::new(config, &arena);
            let program = parse_file(&mut context).context("Compilation failed")?;
            let program = check_all(&mut context, program).context("Compilation error")?;
            let modes = farm_modes(&mut context, &program);
            println!("{:?}", modes);
            Ok(())
        }
    }
}
