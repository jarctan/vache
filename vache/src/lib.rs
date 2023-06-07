//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(array_windows)]
#![feature(iter_collect_into)]
#![feature(default_free_fn)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

mod anf;
pub mod ast;
mod borrowing;
mod compile;
pub mod config;
pub mod context;
pub mod examples;
mod grammar;
mod interpret;
pub mod mir;
mod miring;
mod normalize;
mod tast;
mod typing;
mod utils;

#[cfg(test)]
#[macro_use]
extern crate vache_tests_proc;

#[macro_use]
extern crate quote;

#[macro_use]
extern crate lazy_static;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::path::Path;

use anyhow::Result;
pub use context::Context;
pub use steps::{borrow_check, check, interpret, mir, run};
pub use utils::arena::Arena;

/// Compiles `p` and puts the output program named `name` in `dest_dir`.
pub fn compile<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
    name: impl AsRef<str>,
    dest_dir: &Path,
) {
    let mut checked = check(ctx, p);
    borrow_check(mir(&mut checked));
    steps::cargo(steps::compile(checked), name.as_ref(), dest_dir).unwrap();
}

/// Executes program `p`, returning its standard output.
pub fn execute<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
    name: impl AsRef<str>,
    dest_dir: &Path,
) -> Result<String> {
    let mut checked: tast::Program<'ctx> = check(ctx, p);
    borrow_check(mir(&mut checked));
    let res = steps::run(checked, name, dest_dir)?;
    Ok(res)
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
    use normalize::Normalizer;
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
    pub fn check<'ctx>(
        ctx: &mut Context<'ctx>,
        p: impl Into<ast::Program<'ctx>>,
    ) -> tast::Program<'ctx> {
        let mut typer = Typer::new(ctx);
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
    pub fn borrow_check<'ctx>(p: impl Into<mir::Program<'ctx>>) -> mir::Program<'ctx> {
        let mut borrow_checker = BorrowChecker::new();
        borrow_checker.check(p.into())
    }

    /// Computes the MIR output of a given program.
    ///
    /// Under the hood, in charge of allocating a new `MIRer` and launching it
    /// on your program.
    pub fn mir<'mir>(p: &'mir mut tast::Program<'_>) -> mir::Program<'mir> {
        let mut normalizer = Normalizer::new(p.arena);
        let normalized = normalizer.normalize(p);
        let mut mirer = MIRer::new();
        mirer.gen_mir(normalized)
    }

    /// Compiles a given program.
    ///
    /// Under the hood, in charge of allocating a new `Compiler` and launching
    /// it on your program.
    pub fn compile<'ctx>(p: impl Into<tast::Program<'ctx>>) -> String {
        let mut compiler = Compiler::new();
        compiler.compile(p.into())
    }

    /// Interprets a given program.
    ///
    /// Under the hood, it will allocate a new `Interpreter` and launch it on
    /// your program. It will call the function `main` within your program
    /// and return the standard output of your program.
    pub fn interpret<'ctx>(p: impl Into<mir::Program<'ctx>>) -> String {
        interpret::interpret(p.into())
    }

    /// Runs a given MIR program.
    pub fn run<'ctx>(
        p: impl Into<tast::Program<'ctx>>,
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
        let dest_file = dest_dir.join(name);
        if target_dir.exists() {
            // Check if our metadata file exists within path `target_dir`
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
        if fs::metadata(&directory_to_delete).is_ok() {
            fs::remove_dir_all(directory_to_delete)?;
        }

        // Delete dest_file if it exists
        if fs::metadata(&dest_file).is_ok() {
            fs::remove_file(&dest_file)?;
        }

        // Create /{target_dir}/src
        let file_path = target_dir.join("src/main.rs");
        fs::create_dir_all(file_path.parent().unwrap())?;

        // Write main file
        let mut file = File::create(file_path)?;
        file.write_all(source_code.as_bytes())?;
        file.flush()?;

        // Write config file
        let mut file = File::create(target_dir.join(".vache_info.json"))?;
        file.write_all(b"{\"version\": 1}")?;
        file.flush()?;

        // Write Cargo file
        let mut file = File::create(target_dir.join("Cargo.toml"))?;
        file.write_all(format!("[package]\nname = \"{binary_name}\"\nversion = \"1.0.0\"\nedition = \"2021\"\n\n[dependencies]\n num-bigint = \"0.4.3\"\nnum-traits = \"0.2.15\"\n\n\n[workspace]").as_bytes())?;
        file.flush()?;

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
        fs::copy(source_file, dest_file)?;

        Ok(())
    }
}
