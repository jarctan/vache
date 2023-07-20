//! Defining the compiler config options.

/// Compile configuration.
#[derive(Default)]
pub struct Config<'ctx> {
    /// Compiler input string.
    pub input: &'ctx str,
    /// Compiler input filename.
    pub filename: Option<&'ctx str>,
    /// Do we report invalidations.
    pub report_invalidations: bool,
    /// Verbose mode.
    pub verbose: bool,
}
