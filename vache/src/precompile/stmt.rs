use super::*;

/// Statements in the Rust AST.
#[derive(Debug)]
pub enum Stmt {
    /// Declare a new, uninitialized variable.
    Declare(VarDef),
    /// Assigns a variable.
    Assign(Place, RValue),
    /// Performs a call to `name(args)`, putting the result in variable
    /// `destination`.
    Call {
        /// Name of the function to call.
        name: String,
        /// Arguments to that function.
        args: Vec<VarMode>,
        /// Destination variable to hold the result.
        destination: Option<Var>,
    },
    /// If statement.
    /// * 1st argument: variable to match.
    /// * 2nd argument: true branch.
    /// * 3rd argument: false branch.
    If(VarMode, Vec<Stmt>, Vec<Stmt>),
    /// Endless loop.
    Loop(Vec<Stmt>),
    /// Nested block.
    Block(Vec<Stmt>),
    /// Break instruction.
    Break,
    /// Continue instruction.
    Continue,
}
