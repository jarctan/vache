//! Defining places in the MIR.

use std::fmt;

use super::{Loc, Pointer, VarUse, Varname};
use crate::utils::set::Set;

/// Kinds of places.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Place<'ctx> {
    /// Variable.
    VarP(Varname<'ctx>),
    /// A field in a struct.
    FieldP(Pointer<'ctx>, &'ctx str),
    /// An indexed slot into a location.
    IndexP(Pointer<'ctx>, Pointer<'ctx>),
}

use Place::*;

impl<'ctx> Place<'ctx> {
    /// Get the root location for this place.
    ///
    /// The root location is the most precise location that contains the place.
    pub fn root(&self) -> Loc<'ctx> {
        match self {
            VarP(var) => Loc::VarL(*var),
            FieldP(strukt, field) => Loc::FieldL(strukt.loc(), field),
            IndexP(array, _) => *array.loc(),
        }
    }

    /// Gets the location that is defined (i.e., **entirely** overwritten) by
    /// that place, if it exists.
    pub fn def(&self) -> Option<Loc<'ctx>> {
        (*self).try_into().ok()
    }

    /// Gets the variables used by that place (variables that need to exist
    /// beforehand), if that place is in the left hand side.
    ///
    /// Only difference with rhs is that if the place is a variable, the
    /// variable need not to exist beforehand.
    pub fn uses_as_lhs(&self) -> Set<Loc<'ctx>> {
        match self {
            VarP(_) | FieldP(_, _) => [].into_iter().collect(),
            IndexP(..) => self.uses_as_rhs(),
        }
    }

    /// Gets the variables used by that place (variables that need to exist
    /// beforehand), if that place is in the right hand side.
    pub fn uses_as_rhs(&self) -> Set<Loc<'ctx>> {
        match self {
            VarP(v) => [Loc::VarL(*v)].into_iter().collect(),
            FieldP(strukt, field) => [Loc::FieldL(strukt.loc(), field)].into_iter().collect(),
            IndexP(array, index) => array
                .place()
                .uses_as_rhs()
                .into_iter()
                .chain(index.place().uses_as_rhs().into_iter())
                .collect(),
        }
    }
}

impl<'ctx> From<Varname<'ctx>> for Place<'ctx> {
    fn from(var: Varname<'ctx>) -> Self {
        VarP(var)
    }
}

impl<'a, 'ctx> From<&'a Varname<'ctx>> for Place<'ctx> {
    fn from(var: &'a Varname<'ctx>) -> Self {
        VarP(*var)
    }
}

impl<'ctx> From<VarUse<'ctx>> for Place<'ctx> {
    fn from(var: VarUse<'ctx>) -> Self {
        VarP(var.into())
    }
}

impl<'a, 'ctx> From<&'a VarUse<'ctx>> for Place<'ctx> {
    fn from(var: &'a VarUse<'ctx>) -> Self {
        VarP((*var).into())
    }
}

impl<'a, 'ctx> From<&'a Place<'ctx>> for Place<'ctx> {
    fn from(place: &Place<'ctx>) -> Self {
        *place
    }
}

impl fmt::Debug for Place<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarP(var) => write!(f, "{var:?}"),
            FieldP(pointer, field) => write!(f, "({pointer:?}).{field}"),
            IndexP(array, index) => write!(f, "({array:?})[{index:?}]"),
        }
    }
}
