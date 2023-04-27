//! Interpreter.

use crate::tast::Program;
use string_builder::Builder as StringBuilder;

mod env;
mod interpreter;
mod value;
use interpreter::Interpreter;

use self::env::Env;

/// Runs the interpreter on a given program.
///
/// It will jump to and execute function `main`.
/// Returns the output of the program, as a `String`.
pub fn interpret(p: Program) -> String {
    // Create the interpreter and run it.
    let mut i = Interpreter {
        env: vec![Env::default()],
        fun_env: &p.funs,
        stdout: StringBuilder::default(),
    };
    i.call("main", vec![]);

    i.stdout()
}
