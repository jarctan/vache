//! Defining the compiler config options.

/// Compile configuration.
#[derive(Default)]
pub struct Config<'ctx> {
    /// Compiler input string.
    pub input: &'ctx str,
    /// Compiler input filename.
    pub filename: Option<&'ctx str>,
}
