//! Toy Vache language compiler.

#![feature(try_blocks)]
#![feature(default_free_fn)]

use std::process::Command;

use anyhow::{bail, Context as AnyhowContext, Result};
use clap::{Parser, Subcommand};
use unindent::Unindent;
use vache_lib::{
    borrow_check, check_all, compile, config::Config, examples::parse_file, execute, farm_modes,
    interpret, mir, reporter::Diagnostics, typecheck, Arena, Context,
};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)] // Read from `Cargo.toml`
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

/// Compiler for the Vache programming language.
#[derive(Subcommand, Debug)]
enum Commands {
    /// Gives the MIR output of a program.
    Mir {
        /// The input file to compile.
        filename: String,
        /// Report invalidations as warnings.
        #[arg(short, long)]
        invalidations: bool,
    },
    /// Type-checks a program.
    Check {
        /// The input file to type-check.
        filename: String,
        /// Report invalidations as warnings.
        #[arg(short, long)]
        invalidations: bool,
    },
    /// Compiles a program.
    Compile {
        /// The input file to compile.
        filename: String,
        /// Report invalidations as warnings.
        #[arg(short, long)]
        invalidations: bool,
    },
    /// Runs a program.
    Run {
        /// The input file to compile.
        filename: String,
        /// Report invalidations as warnings.
        #[arg(short, long)]
        invalidations: bool,
    },
    /// Interprets a program.
    Interpret {
        /// The input file to compile.
        filename: String,
        /// Report invalidations as warnings.
        #[arg(short, long)]
        invalidations: bool,
    },
    /// Gets the addressing modes of all elements of the program.
    Modes {
        /// The input file to compile.
        filename: String,
        /// Report invalidations as warnings.
        #[arg(short, long)]
        invalidations: bool,
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
        Commands::Mir {
            ref filename,
            invalidations,
        } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
                report_invalidations: invalidations,
            };
            let mut context = Context::new(config, &arena);

            let res: Result<_> = try {
                let program = parse_file(&mut context)?;
                let mut checked = typecheck(&mut context, program)?;
                let mir = borrow_check(&mut context, mir(&mut checked)?)?;
                println!("{:#?}", mir);

                for (&name, f) in mir.funs.iter() {
                    f.body.print_image(name)?;
                    println!("* CFG of function `{name}` has been saved to `{name}.png`");
                }
            };
            context.reporter.display()?;
            res
        }
        Commands::Check {
            ref filename,
            invalidations,
        } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
                report_invalidations: invalidations,
            };
            let mut context = Context::new(config, &arena);

            let res: Result<_> = try {
                let program = parse_file(&mut context)?;
                check_all(&mut context, program)?;
            };
            context.reporter.display()?;
            res
        }
        Commands::Compile {
            ref filename,
            invalidations,
        } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
                report_invalidations: invalidations,
            };
            let mut context = Context::new(config, &arena);

            let res: Result<_> = try {
                let program = parse_file(&mut context)?;
                compile(&mut context, program, "binary", &cur_dir)?;
            };
            context.reporter.display()?;
            res
        }
        Commands::Run {
            ref filename,
            invalidations,
        } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
                report_invalidations: invalidations,
            };
            let mut context = Context::new(config, &arena);
            let res: Result<_> = try {
                let program = parse_file(&mut context)?;
                execute(&mut context, program, "binary", &cur_dir)?
            };
            context.reporter.display()?;
            match res {
                Ok(res) => {
                    println!("--------------------------------");
                    println!("{}", res);
                    Ok(())
                }
                Err(err) => {
                    bail!(err);
                }
            }
        }
        Commands::Interpret {
            ref filename,
            invalidations,
        } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
                report_invalidations: invalidations,
            };
            let mut context = Context::new(config, &arena);
            let res: Result<_> = try {
                // Compile
                let program = parse_file(&mut context)?;
                let mut checked = typecheck(&mut context, program)?;
                let mir = borrow_check(&mut context, mir(&mut checked)?)?;

                // Interpret
                interpret(mir).context("interpreter error")?
            };
            context.reporter.display()?;
            match res {
                Ok(res) => {
                    println!("--------------------------------");
                    println!("{}", res);
                    Ok(())
                }
                Err(err) => {
                    bail!(err);
                }
            }
        }
        Commands::Modes {
            ref filename,
            invalidations,
        } => {
            let arena = Arena::new();
            let cur_dir = std::env::current_dir().context("Current dir not found")?;
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!("Failed to open file `{filename}` in {}", cur_dir.display())
                })?);
            let config = Config {
                input,
                filename: Some(filename),
                report_invalidations: invalidations,
            };
            let mut context = Context::new(config, &arena);
            let res: Result<_, Diagnostics> = try {
                let program = parse_file(&mut context)?;
                let program = check_all(&mut context, program)?;
                farm_modes(&mut context, &program)
            };
            match res {
                Ok(modes) => {
                    println!("{:?}", modes);
                    Ok(())
                }
                Err(diagnostics) => {
                    diagnostics.display()?;
                    bail!("Collecting modes failed");
                }
            }
        }
    }
}
