//! Defining locations in memory.
//!
//! Locations are the smallest unit that we can reason about. Places are more
//! precise, but we cannot reason about them.
//!
//! Every location is a place. But not every place is a location.

use std::{cmp::Ordering, fmt};

use Place::*;

use super::{Place, Pointer, Reference, VarDef, VarUse, Varname};

/// Location: `var.fields*`. Example: `var.field1.field2.field3`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Loc<'ctx> {
    /// Variable.
    VarL(Varname<'ctx>),
    /// A field in a struct.
    FieldL(&'ctx Loc<'ctx>, &'ctx str),
}

impl<'ctx> Loc<'ctx> {
    /// Number of elements in the path of the location.
    ///
    /// A variable/a field counts towards a single element.
    fn len(&self) -> usize {
        match self {
            VarL(_) => 1,
            FieldL(loc, _) => loc.len() + 1,
        }
    }

    /// Returns the root variable of this location.
    pub fn root(&self) -> Varname<'ctx> {
        match self {
            VarL(var) => *var,
            FieldL(loc, _) => loc.root(),
        }
    }
}

impl<'ctx> PartialOrd for Loc<'ctx> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (VarL(x), VarL(y)) => (x == y).then_some(Ordering::Equal),
            (VarL(_), FieldL(s, _)) => (self <= s).then_some(Ordering::Less),
            (FieldL(s, _), VarL(_)) => (*s >= other).then_some(Ordering::Greater),
            (FieldL(s1, f1), FieldL(s2, f2)) => match self.len().cmp(&other.len()) {
                // If not the same length, shorter the longer length and recursively compare
                Ordering::Less => self.partial_cmp(s2),
                Ordering::Greater => s1.partial_cmp(&other),
                // If they are the same length, then they must be equal
                Ordering::Equal => (s1 == s2 && f1 == f2).then_some(Ordering::Equal),
            },
        }
    }
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

impl<'ctx> From<VarDef<'ctx>> for Loc<'ctx> {
    fn from(vardef: VarDef<'ctx>) -> Self {
        VarL(vardef.name())
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
