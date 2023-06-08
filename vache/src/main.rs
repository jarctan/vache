//! Toy Vache language compiler.

use anyhow::Context as AnyhowContext;
use clap::{Parser, Subcommand};
use vache_lib::{
    borrow_check, check, config::Config, examples::parse_file, mir, run, Arena, Context,
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
}

fn main() -> anyhow::Result<()> {
    /*let mut checked = check(vache_lib::examples::assignment_while_borrowed());
    let mir = borrow_check(mir(&mut checked));
    println!("{mir:?}");
    let res = run(checked, "binary", &std::env::current_dir().unwrap()).expect("error");
    println!("{}", res);*/
    match Cli::parse().command {
        Commands::Compile { ref filename } => {
            let arena = Arena::new();
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!(
                        "Failed to open file `{filename}` in {}",
                        std::env::current_dir().unwrap_or_default().display()
                    )
                })?);
            let config = Config { input };
            let mut context = Context::new(config, &arena);

            let program = parse_file(&mut context).context("Compilation failed")?;
            let mut checked = check(&mut context, program);
            let mir = borrow_check(mir(&mut checked));
            println!("{mir:?}");
        }
        Commands::Run { ref filename } => {
            let arena = Arena::new();
            let input: &str =
                arena.alloc(std::fs::read_to_string(filename).with_context(|| {
                    format!(
                        "Failed to open file `{filename}` in {}",
                        std::env::current_dir().unwrap_or_default().display()
                    )
                })?);
            let config = Config { input };
            let mut context = Context::new(config, &arena);
            let program = parse_file(&mut context).context("Compilation failed")?;
            let mut checked = check(&mut context, program);
            let mir = borrow_check(mir(&mut checked));
            println!("{mir:?}");
            let res = run(checked, "binary", &std::env::current_dir()?).context("runtime error")?;
            println!("{}", res);
        }
    }
    Ok(())
}
