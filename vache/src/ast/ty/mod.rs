//! Parsing types in their different shapes, and defining their representation
//! in the AST.

mod subst;
#[allow(clippy::module_inception)]
mod ty;
mod tyuse;
mod var;

pub use subst::TySubst;
pub use ty::Ty;
pub use tyuse::TyUse;
pub use var::TyVar;
use Ty::*;

/// Shortcut for the `UnitT` without any span context.
#[allow(non_snake_case)]
pub fn unitT<'ctx>() -> TyUse<'ctx> {
    UnitT.into()
}

/// Shortcut for the `BoolT` without any span context.
#[allow(non_snake_case)]
pub fn boolT<'ctx>() -> TyUse<'ctx> {
    BoolT.into()
}

/// Shortcut for the `StrT` without any span context.
#[allow(non_snake_case)]
pub fn strT<'ctx>() -> TyUse<'ctx> {
    StrT.into()
}

/// Shortcut for the `IntT` without any span context.
#[allow(non_snake_case)]
pub fn intT<'ctx>() -> TyUse<'ctx> {
    IntT.into()
}

/// Shortcut for the `VarT` without any span context.
#[allow(non_snake_case)]
pub fn varT(name: &str) -> TyUse<'_> {
    VarT(TyVar::Named(name)).into()
}

/// Shortcut for the `ArrayT` without any span context.
#[allow(non_snake_case)]
pub fn arrayT<'ctx>(item: &'ctx Ty<'ctx>) -> TyUse<'ctx> {
    ArrayT(item).into()
}
