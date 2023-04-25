mod basic_lifetime;
mod ceil_mod2;
mod fibo;
mod scopes;
mod simple;

use super::*;

fn test(p: Program) {
    crate::check(p);
}
