//! Defining the arena interface used by the compiler.

use std::collections::HashMap;
use std::fmt;

use codespan_reporting::files::SimpleFile as SimpleFileReporting;
use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::Config as TermConfig;
use colosseum::sync;

use crate::anf::{Loc, Place};
use crate::ast::{Enum, FunSig, Namespace, Struct, Ty};

/// Can be stored in and retrieved from our arena.
pub trait Arenable<'ctx> {
    /// Transforms into an arena stored element.
    fn into(self) -> ArenaElem<'ctx>;
    /// Gets a reference into the object associated with an arena stored
    /// element.
    fn as_ref<'a>(arena: &'a ArenaElem<'ctx>) -> &'a Self;
    /// Gets a mutable reference into the object associated with an arena stored
    /// element.
    fn as_mut<'a>(arena: &'a mut ArenaElem<'ctx>) -> &'a mut Self;
}

/// Provides the enumeration of supported types by the Arena.
macro_rules! arenable_elements {
    ($($name:ident: $ty:ty),*) => {
        pub enum ArenaElem<'ctx> {
            $($name(Box<$ty>)),*
        }
        $(impl<'ctx> Arenable<'ctx> for $ty {
            fn into(self) -> ArenaElem<'ctx> {
                ArenaElem::$name(Box::new(self))
            }

            fn as_ref<'a>(arena: &'a ArenaElem<'ctx>) -> &'a Self {
                match arena {
                    ArenaElem::$name(box ref e) => e,
                    _ => panic!(),
                }
            }

            fn as_mut<'a>(arena: &'a mut ArenaElem<'ctx>) -> &'a mut Self {
                match arena {
                    ArenaElem::$name(box ref mut e) => e,
                    _ => panic!(),
                }
            }
        })*
    };
}

arenable_elements!(
    StandardStream: StandardStream,
    TermConfig: TermConfig,
    SimpleFileReporting: SimpleFileReporting<&'ctx str, &'ctx str>,
    String: String,
    Ty: Ty<'ctx>,
    VecTy: Vec<Ty<'ctx>>,
    Namespace: Namespace<'ctx>,
    Place: Place<'ctx>,
    FunSig: FunSig<'ctx>,
    Struct: Struct<'ctx>,
    Enum: Enum<'ctx>,
    HashMapStruct: HashMap<&'ctx str, Struct<'ctx>>,
    HashMapEnum: HashMap<&'ctx str, Enum<'ctx>>,
    Loc: Loc<'ctx>
);

/// Compiler arena.
pub struct Arena<'ctx>(sync::Arena<ArenaElem<'ctx>>);

impl<'ctx> Arena<'ctx> {
    /// Creates a new arena.
    pub fn new() -> Arena<'ctx> {
        Arena(sync::Arena::new())
    }

    /// Allocates a new element in the arena.
    pub fn alloc<T: Arenable<'ctx>>(&self, t: T) -> &T {
        Arenable::as_ref(self.0.alloc(t.into()))
    }

    /// Allocates a new element in the arena and returns a mutable reference
    /// into it.
    pub fn alloc_mut<T: Arenable<'ctx>>(&self, t: T) -> &mut T {
        Arenable::as_mut(self.0.alloc(t.into()))
    }
}

impl fmt::Debug for Arena<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Arena").finish_non_exhaustive()
    }
}

impl Default for Arena<'_> {
    fn default() -> Self {
        Self::new()
    }
}
