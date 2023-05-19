//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(array_windows)]
#![feature(iter_collect_into)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

pub mod ast;
mod borrowing;
mod compile;
pub mod examples;
mod interpret;
pub mod mir;
mod miring;
mod precompile;
mod tast;
mod typing;
mod utils;

#[macro_use]
extern crate quote;

use std::{io, path::Path};

pub use steps::{borrow_check, check, interpret, mir, run};

/// Compiles `p` and puts the output program named `name` in `dest_dir`.
pub fn compile(p: impl Into<ast::Program>, name: impl AsRef<str>, dest_dir: &Path) {
    steps::cargo(
        steps::compile(borrow_check(mir(check(p)))),
        name.as_ref(),
        dest_dir,
    )
    .unwrap();
}

/// Executes program `p`, returning its standard output.
pub fn execute(
    p: impl Into<ast::Program>,
    name: impl AsRef<str>,
    dest_dir: &Path,
) -> io::Result<String> {
    steps::run(borrow_check(mir(check(p))), name, dest_dir)
}

mod steps {
    //! Defining the function for all the steps of the compiler.
    //!
    //! Only available throughout the

    use std::fs::{self, File};
    use std::io::{self, Write};
    use std::path::Path;
    use std::process::Command;

    use borrowing::BorrowChecker;
    use compile::Compiler;
    use miring::MIRer;
    use typing::Typer;

    use super::*;

    /// Checks a given program.
    ///
    /// If it returns successfully, the program type-checked. Otherwise, it will
    /// panic (note: temporary behavior, there should of course be no panicking
    /// in the future, only `Result`s).
    ///
    /// Under the hood, this function is in charge of allocating a new `Typer`
    /// and launching it on your program.
    pub fn check(p: impl Into<ast::Program>) -> tast::Program {
        let mut typer = Typer::new();
        typer.check(p.into())
    }

    /// Borrow-checks a given program.
    ///
    /// If it returns successfully, the program borrow-checked. Otherwise, it
    /// will panic (note: temporary behavior, there should of course be no
    /// panicking in the future, only `Result`s).
    ///
    /// Under the hood, this function is in charge of allocating a new
    /// `BorrowChecker` and launching it on your program.
    pub fn borrow_check(p: impl Into<mir::Program>) -> mir::Program {
        let mut borrow_checker = BorrowChecker::new();
        borrow_checker.check(p.into())
    }

    /// Computes the MIR output of a given program.
    ///
    /// Under the hood, in charge of allocating a new `MIRer` and launching it
    /// on your program.
    pub fn mir(p: impl Into<tast::Program>) -> mir::Program {
        let mut mirer = MIRer::new();
        mirer.gen_mir(p.into())
    }

    /// Compiles a given program.
    ///
    /// Under the hood, in charge of allocating a new `Compiler` and launching
    /// it on your program.
    pub fn compile(p: impl Into<mir::Program>) -> String {
        let mut compiler = Compiler::new();
        compiler.compile(p.into())
    }

    /// Interprets a given program.
    ///
    /// Under the hood, it will allocate a new `Interpreter` and launch it on
    /// your program. It will call the function `main` within your program
    /// and return the standard output of your program.
    pub fn interpret(p: impl Into<mir::Program>) -> String {
        interpret::interpret(p.into())
    }

    /// Runs a given MIR program.
    pub fn run(
        p: impl Into<mir::Program>,
        name: impl AsRef<str>,
        dest_dir: &Path,
    ) -> io::Result<String> {
        let name = name.as_ref();
        cargo(compile(p), name, dest_dir);
        // Cargo run on the file
        Command::new(format!("./{name}"))
            .current_dir(dest_dir)
            .output()
            .map(|output| String::from_utf8(output.stdout).unwrap())
    }

    /// Final stage: compiles the Rust source code down to machine code.
    pub fn cargo(source: String, name: &str, dest_dir: &Path) -> io::Result<()> {
        let file_path = Path::new("template/src/main.rs");
        let mut file = File::create(file_path)?;
        file.write_all(source.as_bytes())?;

        // Cargo run on the file
        Command::new("cargo ")
            .current_dir("template")
            .arg("run")
            .arg("--release")
            .output()?;

        // Copy and paste another file to a new directory
        let source_file = Path::new("template/target/release/binary");
        fs::create_dir_all(dest_dir)?;

        let dest_file = dest_dir.join(name);
        fs::copy(source_file, dest_file)?;

        Ok(())
    }
}
