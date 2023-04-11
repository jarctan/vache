/// A compilable item.
pub trait Compilable {
    /// Compile the item.
    fn compile(self, c: &mut Compiler) -> String;
}

/// Compiler, that turns our language into source code for an
/// executable language.
pub struct Compiler {}
impl Compiler {
    /// Creates a new compiler.
    pub fn new() -> Self {
        Self {}
    }

    /// Compiles a compilable item into an executable source code.
    pub fn compile(&mut self, c: impl Compilable) -> String {
        c.compile(self)
    }
}
