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
    use std::io::{self, ErrorKind, Write};
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
        cargo(compile(p), name, dest_dir)?;

        // Cargo run on the file
        let run_cmd = Command::new(format!("./{name}"))
            .current_dir(dest_dir)
            .output()?;

        if run_cmd.status.success() {
            Ok(String::from_utf8(run_cmd.stdout).unwrap())
        } else {
            Err(io::Error::new(ErrorKind::Other, "Your program failed"))
        }
    }

    /// Final stage: compiles the Rust source code down to machine code.
    pub fn cargo(source_code: String, name: &str, dest_dir: &Path) -> io::Result<()> {
        let target_dir = Path::new("vache_target");
        let binary_name = "binary";
        if target_dir.exists() {
            // Check if the file exists within path A
            let file_path = target_dir.join(".vache_info.json");
            if !file_path.is_file() {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!(
                        "`{}` already exists but do not contain `{}`. Will not overwrite it.",
                        target_dir.to_str().unwrap(),
                        file_path.file_name().unwrap().to_str().unwrap()
                    ),
                ));
            }
        }

        // Delete `/{target_dir}/src` if it exists
        let directory_to_delete = target_dir.join("src");
        fs::remove_dir_all(directory_to_delete)?;

        // Create /{target_dir}/src
        let file_path = target_dir.join("src/main.rs");
        fs::create_dir_all(file_path.parent().unwrap())?;

        // Write main file
        let mut file = File::create(file_path)?;
        file.write_all(source_code.as_bytes())?;

        // Write config file
        let mut file = File::create(target_dir.join(".vache_info.json"))?;
        file.write_all(b"{\"version\": 1}")?;

        // Write Cargo file
        let mut file = File::create(target_dir.join("Cargo.toml"))?;
        file.write_all(format!("[package]\nname = \"{binary_name}\"\nversion = \"1.0.0\"\nedition = \"2021\"\n\n[dependencies]\nrug = \"1.19.2\"\n\n[workspace]").as_bytes())?;

        // Cargo run on the file
        let cargo_cmd = Command::new("cargo")
            .current_dir(target_dir)
            .arg("run")
            .arg("--release")
            .output()?;
        if !cargo_cmd.status.success() {
            eprintln!("{}", String::from_utf8(cargo_cmd.stdout).unwrap());
            eprintln!("{}", String::from_utf8(cargo_cmd.stderr).unwrap());
            return Err(io::Error::new(ErrorKind::Other, "Cargo compilation failed"));
        }

        // Copy and paste another file to a new directory
        let source_file = target_dir.join(format!("target/release/{binary_name}"));
        fs::create_dir_all(dest_dir)?;
        let dest_file = dest_dir.join(name);
        fs::copy(source_file, dest_file)?;

        Ok(())
    }
}