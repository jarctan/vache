//! Defining locations in memory.
//!
//! Locations are the smallest unit that we can reason about. Places are more
//! precise, but we cannot reason about them.
//!
//! Every location is a place. But not every place is a location.

use std::fmt;

use Place::*;

use super::{Place, Pointer, Reference, VarUse, Varname};

/// Location: `var.fields*`. Example: `var.field1.field2.field3`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Loc<'ctx> {
    /// Variable.
    VarL(Varname<'ctx>),
    /// A field in a struct.
    FieldL(&'ctx Loc<'ctx>, &'ctx str),
}

use Loc::*;

impl<'ctx> TryFrom<Place<'ctx>> for Loc<'ctx> {
    type Error = ();

    fn try_from(value: Place<'ctx>) -> Result<Self, Self::Error> {
        match value {
            VarP(var) => Ok(VarL(var)),
            FieldP(strukt, field) => Ok(FieldL(strukt.loc(), field)),
            IndexP(_, _) => Err(()),
        }
    }
}

impl<'a, 'ctx> From<&'a Self> for Loc<'ctx> {
    fn from(loc: &Self) -> Self {
        *loc
    }
}

impl<'ctx> From<Varname<'ctx>> for Loc<'ctx> {
    fn from(var: Varname<'ctx>) -> Self {
        VarL(var)
    }
}

impl<'a, 'ctx> From<&'a Varname<'ctx>> for Loc<'ctx> {
    fn from(var: &Varname<'ctx>) -> Self {
        VarL(*var)
    }
}

impl<'ctx> From<VarUse<'ctx>> for Loc<'ctx> {
    fn from(var: VarUse<'ctx>) -> Self {
        VarL(var.into())
    }
}

impl<'a, 'ctx> From<&'a VarUse<'ctx>> for Loc<'ctx> {
    fn from(var: &VarUse<'ctx>) -> Self {
        VarL((*var).into())
    }
}

impl<'a, 'mir, 'ctx> From<&'a Reference<'mir, 'ctx>> for Loc<'ctx> {
    fn from(reference: &'a Reference<'mir, 'ctx>) -> Self {
        *reference.loc()
    }
}

impl<'a, 'ctx> From<&'a Pointer<'ctx>> for Loc<'ctx> {
    fn from(ptr: &'a Pointer<'ctx>) -> Self {
        *ptr.loc()
    }
}

impl fmt::Display for Loc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarL(var) => write!(f, "{}", var),
            FieldL(loc, field) => write!(f, "{}.{}", loc, field),
        }
    }
}

impl fmt::Debug for Loc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
