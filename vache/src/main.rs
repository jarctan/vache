//! Toy Vache language compiler.

#![feature(try_blocks)]

use std::process::Command;

use anyhow::{bail, Context as AnyhowContext};
use clap::{Parser, Subcommand};
use unindent::Unindent;
use vache_lib::{
    borrow_check, check_all, compile, config::Config, examples::parse_file, execute, farm_modes,
    interpret, mir, reporter::Diagnostics, typecheck, Arena, Context,
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
    /// Type-checks a program.
    Check {
        /// The input file to type-check.
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
    /// Interprets a program.
    Interpret {
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

            let res: Result<_, Diagnostics> = try {
                let program = parse_file(&mut context)?;
                let mut checked = typecheck(&mut context, program)?;
                let mir = borrow_check(&mut context, mir(&mut checked)?)?;
                println!("{:#?}", mir);

                for (&name, f) in mir.funs.iter() {
                    f.body.print_image(name)?;
                    println!("* CFG of function `{name}` has been saved to `{name}.png`");
                }
            };
            if let Err(diagnostics) = res {
                diagnostics.display()?;
                bail!("Compile errors found");
            }
            Ok(())
        }
        Commands::Check { ref filename } => {
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

            let res: Result<_, Diagnostics> = try {
                let program = parse_file(&mut context)?;
                check_all(&mut context, program)?;
            };
            if let Err(diagnostics) = res {
                diagnostics.display()?;
                bail!("Compile errors found");
            }
            Ok(())
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

            let res: Result<_, Diagnostics> = try {
                let program = parse_file(&mut context)?;
                compile(&mut context, program, "binary", &cur_dir)?;
            };
            if let Err(diagnostics) = res {
                diagnostics.display()?;
                bail!("Compilation failed");
            }
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
            let res: Result<_, Diagnostics> = try {
                let program = parse_file(&mut context)?;
                execute(&mut context, program, "binary", &cur_dir)?
            };
            match res {
                Ok(res) => {
                    println!("--------------------------------");
                    println!("{}", res);
                    Ok(())
                }
                Err(diagnostics) => {
                    diagnostics.display()?;
                    bail!("Execution failed");
                }
            }
        }
        Commands::Interpret { ref filename } => {
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
            let res: Result<_, Diagnostics> = try {
                // Compile
                let program = parse_file(&mut context)?;
                let mut checked = typecheck(&mut context, program)?;
                let mir = borrow_check(&mut context, mir(&mut checked)?)?;

                // Interpret
                interpret(mir).context("interpreter error")?
            };
            match res {
                Ok(res) => {
                    println!("--------------------------------");
                    println!("{}", res);
                    Ok(())
                }
                Err(diagnostics) => {
                    diagnostics.display()?;
                    bail!("Interpretation failed");
                }
            }
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
