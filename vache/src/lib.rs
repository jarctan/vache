//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(array_windows)]
#![feature(iter_collect_into)]
#![feature(default_free_fn)]
#![feature(iter_array_chunks)]
#![feature(try_blocks)]
#![feature(if_let_guard)]
#![feature(step_trait)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use std::time::Instant;

mod anf;
pub mod ast;
mod borrowing;
pub mod codes;
mod compile;
pub mod config;
pub mod context;
pub mod examples;
mod grammar;
mod interpret;
pub mod mir;
mod miring;
mod mode_farmer;
mod normalize;
mod prelude;
pub mod reporter;
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

#[macro_use]
extern crate anyhow;

use std::path::Path;

use anyhow::Result;
pub use context::Context;
pub use mode_farmer::farm_modes;
use prelude::prelude;
pub use steps::{borrow_check, interpret, mir, run, typecheck};
pub use utils::arena::Arena;

/// Compiles `p` and puts the output program named `name` in `dest_dir`.
pub fn compile<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
    name: impl AsRef<str>,
    dest_dir: &Path,
) -> Result<()> {
    match typecheck(ctx, p)? {
        Ok(mut checked) => {
            if let Err(diagnostics) = borrow_check(ctx, mir(&mut checked)?)? {
                diagnostics.display()?;
                bail!("Compile errors found");
            }
            steps::cargo(steps::compile(ctx, checked)?, name.as_ref(), dest_dir).unwrap();
            Ok(())
        }
        Err(diagnostics) => {
            diagnostics.display()?;
            bail!("Compile errors found");
        }
    }
}

/// Type and borrow-checks `p`, returning the final typed AST.
pub fn check_all<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
) -> Result<tast::Program<'ctx>> {
    match typecheck(ctx, p)? {
        Ok(mut checked) => {
            if let Err(diagnostics) = borrow_check(ctx, mir(&mut checked)?)? {
                diagnostics.display()?;
                bail!("Compile errors found");
            }
            Ok(checked)
        }
        Err(diagnostics) => {
            diagnostics.display()?;
            bail!("Compile errors found");
        }
    }
}

/// Executes program `p`, returning its standard output.
pub fn execute<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
    name: impl AsRef<str>,
    dest_dir: &Path,
) -> Result<String> {
    match typecheck(&mut *ctx, p)? {
        Ok(mut checked) => {
            if let Err(diagnostics) = borrow_check(&mut *ctx, mir(&mut checked)?)? {
                diagnostics.display()?;
                bail!("Compile errors found");
            }
            let res = steps::run(ctx, checked, name, dest_dir)?;
            Ok(res)
        }
        Err(diagnostics) => {
            diagnostics.display()?;
            bail!("Compile errors found");
        }
    }
}

mod steps {
    //! Defining the function for all the steps of the compiler.
    //!
    //! Only available throughout the

    use std::fs::{self, File};
    use std::io::Write;
    use std::path::Path;
    use std::process::Command;

    use borrowing::BorrowChecker;
    use compile::Compiler;
    use miring::MIRer;
    use normalize::Normalizer;
    use typing::Typer;
    use unindent::Unindent;

    use super::*;
    use crate::reporter::Diagnostics;

    /// Checks a given program.
    ///
    /// If it returns successfully, the program type-checked. Otherwise, it will
    /// panic (note: temporary behavior, there should of course be no panicking
    /// in the future, only `Result`s).
    ///
    /// Under the hood, this function is in charge of allocating a new `Typer`
    /// and launching it on your program.
    pub fn typecheck<'a, 'ctx>(
        ctx: &'a mut Context<'ctx>,
        p: impl Into<ast::Program<'ctx>>,
    ) -> Result<Result<tast::Program<'ctx>, Diagnostics<'ctx>>> {
        print!("Type-checking...");
        std::io::stdout().flush()?;
        let start = Instant::now();

        let mut typer = Typer::new(ctx);
        let res = typer.check(p.into());
        let res = if ctx.reporter.has_errors() {
            Ok(Err(ctx.reporter.flush()))
        } else {
            Ok(Ok(res))
        };
        println!("\rType-checked [{:?}]", start.elapsed());
        res
    }

    /// Borrow-checks a given program.
    ///
    /// If it returns successfully, the program borrow-checked. Otherwise, it
    /// will panic (note: temporary behavior, there should of course be no
    /// panicking in the future, only `Result`s).
    ///
    /// Under the hood, this function is in charge of allocating a new
    /// `BorrowChecker` and launching it on your program.
    pub fn borrow_check<'a, 'mir, 'ctx>(
        ctx: &'a mut Context<'ctx>,
        p: impl Into<mir::Program<'mir>>,
    ) -> Result<Result<mir::Program<'mir>, Diagnostics<'ctx>>> {
        print!("Borrow-checking...");
        let p = p.into();
        std::io::stdout().flush()?;
        let start = Instant::now();
        let mut borrow_checker = BorrowChecker::new();
        let res = borrow_checker.check(ctx, p);
        println!("\rBorrow-checked [{:?}]", start.elapsed());
        Ok(res)
    }

    /// Computes the MIR output of a given program.
    ///
    /// Under the hood, in charge of allocating a new `MIRer` and launching it
    /// on your program.
    pub fn mir<'mir>(p: &'mir mut tast::Program<'_>) -> Result<mir::Program<'mir>> {
        print!("Miring...");
        std::io::stdout().flush()?;
        let start = Instant::now();
        let normalized = Normalizer::normalize(p);
        let mut mirer = MIRer::new();
        let res = mirer.gen_mir(normalized);
        println!("\rMIR-ed [{:?}]", start.elapsed());
        Ok(res)
    }

    /// Compiles a given program.
    ///
    /// Under the hood, in charge of allocating a new `Compiler` and launching
    /// it on your program.
    pub fn compile<'ctx>(
        ctx: &mut Context<'ctx>,
        p: impl Into<tast::Program<'ctx>>,
    ) -> Result<String> {
        print!("Translating into Rust code...");
        std::io::stdout().flush()?;
        let start = Instant::now();

        let mut compiler = Compiler::new(ctx);
        let res = compiler.compile(p.into());
        println!("\rTranslated into Rust code [{:?}]", start.elapsed());
        Ok(res)
    }

    /// Interprets a given program.
    ///
    /// Under the hood, it will allocate a new `Interpreter` and launch it on
    /// your program. It will call the function `main` within your program
    /// and return the standard output of your program.
    pub fn interpret<'ctx>(p: impl Into<mir::Program<'ctx>>) -> Result<String> {
        Ok(interpret::interpret(p.into()))
    }

    /// Runs a given MIR program.
    pub fn run<'ctx>(
        ctx: &mut Context<'ctx>,
        p: impl Into<tast::Program<'ctx>>,
        name: impl AsRef<str>,
        dest_dir: &Path,
    ) -> Result<String> {
        let name = name.as_ref();
        cargo(compile(ctx, p)?, name, dest_dir)?;

        println!("--------------------------------");
        // Cargo run on the file
        let run_cmd = Command::new(format!("./{name}"))
            .current_dir(dest_dir)
            .output()?;

        if run_cmd.status.success() {
            Ok(String::from_utf8(run_cmd.stdout).unwrap())
        } else {
            Err(anyhow!("Program terminated with {}", run_cmd.status)
                .context(::std::string::String::from_utf8(run_cmd.stderr).unwrap()))
        }
    }

    /// Final stage: compiles the Rust source code down to machine code.
    pub fn cargo(source_code: String, name: &str, dest_dir: &Path) -> Result<()> {
        print!("Compiling the Rust code...");
        std::io::stdout().flush()?;
        let start = Instant::now();

        let target_dir = Path::new("vache_target");
        let binary_name = "binary";
        let dest_file = dest_dir.join(name);
        if target_dir.exists() {
            // Check if our metadata file exists within path `target_dir`
            let file_path = target_dir.join(".vache_info.json");
            if !file_path.is_file() {
                bail!(
                    "`{}` already exists but do not contain `{}`. Will not overwrite it.",
                    target_dir.to_str().unwrap(),
                    file_path.file_name().unwrap().to_str().unwrap()
                );
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
        file.write_all(
            format!(
                "
        [package]
        name = \"{binary_name}\"
        version = \"1.0.0\"
        edition = \"2021\"
        
        [dependencies]
        num-bigint = \"0.4.3\"
        num-traits = \"0.2.15\"
        thiserror = \"1.0.40\"
        anyhow = \"1.0.71\"
        
        [workspace]"
            )
            .unindent()
            .as_bytes(),
        )?;
        file.flush()?;

        // Write Cargo toolchain file.
        let mut file = File::create(target_dir.join("rust-toolchain.toml"))?;
        file.write_all(
            "
            [toolchain]
            channel = \"nightly-2023-06-01\"
        "
            .unindent()
            .as_bytes(),
        )?;
        file.flush()?;

        // Cargo run on the file
        let cargo_cmd = Command::new("cargo")
            .current_dir(target_dir)
            .arg("build")
            .arg("--release")
            .arg("-q")
            .output()?;

        if !cargo_cmd.status.success() {
            bail!(
                "Cargo compilation failed\n{}\n{}",
                String::from_utf8(cargo_cmd.stdout).unwrap(),
                String::from_utf8(cargo_cmd.stderr).unwrap()
            );
        }

        // Copy and paste another file to a new directory
        let source_file = target_dir.join(format!("target/release/{binary_name}"));
        fs::create_dir_all(dest_dir)?;
        fs::copy(source_file, dest_file)?;

        println!("\rCompiled the Rust code [{:?}]", start.elapsed());
        Ok(())
    }
}
