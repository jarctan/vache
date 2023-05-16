mod arrays;
mod basic_lifetime;
mod ceil_mod2;
mod fibo;
mod mutual_structs;
mod scopes;
mod simple;
mod structures;
mod unknown_struct_name;

use super::*;

fn test(p: impl Into<Program>) {
    crate::check(p.into());
}
