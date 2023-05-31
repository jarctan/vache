//! Toy Vache language compiler.

use clap::{Parser, Subcommand};
use vache_lib::{borrow_check, check, examples::parse_file, mir, run};

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

fn main() {
    /*let mut checked = check(vache_lib::examples::assignment_while_borrowed());
    let mir = borrow_check(mir(&mut checked));
    println!("{mir:?}");
    let res = run(checked, "binary", &std::env::current_dir().unwrap()).expect("error");
    println!("{}", res);*/
    match Cli::parse().command {
        Commands::Compile { filename } => {
            let (program, _) = parse_file(&filename).unwrap();
            let mut checked = check(program);
            let mir = borrow_check(mir(&mut checked));
            println!("{mir:?}");
        }
        Commands::Run { filename } => {
            let (program, _) = parse_file(&filename).unwrap();
            let mut checked = check(program);
            let mir = borrow_check(mir(&mut checked));
            println!("{mir:?}");
            let res = run(checked, "binary", &std::env::current_dir().unwrap()).expect("error");
            println!("{}", res);
        }
    }
}
