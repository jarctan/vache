//! Defining the compiler config options.

/// Compile configuration.
pub struct Config<'ctx> {
    /// Compiler input string.
    pub input: &'ctx str,
}
