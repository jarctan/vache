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

use std::borrow::Borrow;
use std::time::Instant;

// Defined first so that the macros can be used in the other modules.
#[macro_use]
pub mod context;

mod anf;
pub mod ast;
mod borrowing;
pub mod codes;
mod compile;
pub mod config;
pub mod examples;
mod grammar;
mod interpret;
mod lft_name_gen;
pub mod mir;
mod miring;
mod mode_farmer;
mod normalize;
mod prelude;
pub mod reporter;
mod scoped;
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
use scoped::Scoped;
pub use steps::{borrow_check, interpret, mir, run, typecheck};
pub use utils::arena::Arena;

/// Compiles `p` and puts the output program named `name` in `dest_dir`.
pub fn compile<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
    name: impl Borrow<str>,
    dest_dir: &Path,
) -> Result<()> {
    let mut checked = typecheck(ctx, p)?;
    let mired = mir(ctx, &mut checked)?;
    borrow_check(&mut *ctx, mired)?;
    let compiled = steps::compile(ctx, checked)?;
    steps::cargo(ctx, compiled, name.borrow(), dest_dir).unwrap();
    Ok(())
}

/// Type and borrow-checks `p`, returning the final typed AST.
pub fn check_all<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
) -> Result<tast::Program<'ctx>> {
    let mut checked = typecheck(ctx, p)?;
    let mired = mir(ctx, &mut checked)?;
    borrow_check(ctx, mired)?;
    Ok(checked)
}

/// Executes program `p`, returning its standard output.
pub fn execute<'ctx>(
    ctx: &mut Context<'ctx>,
    p: impl Into<ast::Program<'ctx>>,
    name: impl Borrow<str>,
    dest_dir: &Path,
) -> Result<String> {
    let mut checked = typecheck(&mut *ctx, p)?;
    let mired = mir(ctx, &mut checked)?;
    borrow_check(&mut *ctx, mired)?;
    let res = steps::run(ctx, checked, name, dest_dir)?;
    Ok(res)
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
    ) -> Result<tast::Program<'ctx>> {
        verbose_print!(ctx, "Type-checking...");
        std::io::stdout().flush()?;
        let start = Instant::now();

        let mut typer = Typer::new(ctx);
        let res = typer.check(p.into());
        let res = if ctx.reporter.has_errors() {
            Err(anyhow!("found type errors"))
        } else {
            Ok(res)
        };
        verbose_println!(ctx, "\rType-checked [{:?}]", start.elapsed());
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
        p: impl Into<mir::Program<'mir, 'ctx>>,
    ) -> Result<mir::Program<'mir, 'ctx>> {
        verbose_print!(ctx, "Borrow-checking...");
        let p = p.into();
        std::io::stdout().flush()?;
        let start = Instant::now();
        let mut borrow_checker = BorrowChecker::new();
        let res = borrow_checker.check(ctx, p);
        verbose_println!(ctx, "\rBorrow-checked [{:?}]", start.elapsed());
        res
    }

    /// Computes the MIR output of a given program.
    ///
    /// Under the hood, in charge of allocating a new `MIRer` and launching it
    /// on your program.
    pub fn mir<'ctx, 'mir>(
        ctx: &mut Context<'ctx>,
        p: &'mir mut tast::Program<'ctx>,
    ) -> Result<mir::Program<'mir, 'ctx>> {
        verbose_print!(ctx, "Miring...");
        std::io::stdout().flush()?;
        let start = Instant::now();
        let arena = p.arena;
        let normalized = Normalizer::normalize(p);
        let mut mirer = MIRer::new(arena);
        let res = mirer.gen_mir(normalized);
        verbose_println!(ctx, "\rMIR-ed [{:?}]", start.elapsed());
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
        verbose_print!(ctx, "Translating into Rust code...");
        std::io::stdout().flush()?;
        let start = Instant::now();

        let mut compiler = Compiler::new(ctx);
        let res = compiler.compile(p.into());
        verbose_println!(ctx, "\rTranslated into Rust code [{:?}]", start.elapsed());
        Ok(res)
    }

    /// Interprets a given program.
    ///
    /// Under the hood, it will allocate a new `Interpreter` and launch it on
    /// your program. It will call the function `main` within your program
    /// and return the standard output of your program.
    pub fn interpret<'mir, 'ctx: 'mir>(p: impl Into<mir::Program<'mir, 'ctx>>) -> Result<String> {
        Ok(interpret::interpret(p.into()))
    }

    /// Runs a given MIR program.
    pub fn run<'ctx>(
        ctx: &mut Context<'ctx>,
        p: impl Into<tast::Program<'ctx>>,
        name: impl Borrow<str>,
        dest_dir: &Path,
    ) -> Result<String> {
        let name = name.borrow();
        let compiled = compile(ctx, p)?;
        cargo(ctx, compiled, name, dest_dir)?;

        // Cargo run on the file
        verbose_print!(ctx, "Running the program...");
        std::io::stdout().flush()?;
        let start = ::std::time::Instant::now();
        let run_cmd = Command::new(format!("./{name}"))
            .current_dir(dest_dir)
            .output()?;
        verbose_println!(ctx, "\rRan the program in [{:?}]", start.elapsed());

        if run_cmd.status.success() {
            Ok(String::from_utf8(run_cmd.stdout).unwrap())
        } else {
            Err(anyhow!("Program terminated with {}", run_cmd.status)
                .context(::std::string::String::from_utf8(run_cmd.stderr).unwrap()))
        }
    }

    /// Final stage: compiles the Rust source code down to machine code.
    pub fn cargo(
        ctx: &mut Context<'_>,
        source_code: String,
        name: &str,
        dest_dir: &Path,
    ) -> Result<()> {
        verbose_print!(ctx, "Compiling the Rust code...");
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

        [profile.release]
        lto = true
        
        [dependencies]
        rand = \"0.8.5\"
        thiserror = \"1.0.40\"
        anyhow = \"1.0.71\"

        [dependencies.malachite]
        version = \"0.3.0\"
        default-features = false
        features = [\"naturals_and_integers\"]
        
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
            bail!(anyhow!("cargo compilation failed").context(format!(
                "\n{}\n{}",
                String::from_utf8(cargo_cmd.stdout).unwrap(),
                String::from_utf8(cargo_cmd.stderr).unwrap()
            )));
        }

        // Copy and paste another file to a new directory
        let source_file = target_dir.join(format!("target/release/{binary_name}"));
        fs::create_dir_all(dest_dir)?;
        fs::copy(source_file, dest_file)?;

        verbose_println!(ctx, "\rCompiled the Rust code [{:?}]", start.elapsed());
        Ok(())
    }
}
