mod arrays;
mod basic_lifetime;
mod ceil_mod2;
mod fibo;
mod mutual_structs;
mod scopes;
mod simple;
mod structures;
mod unknown_struct_name;

use std::default::default;

use ::vache_lib::{config::Config, Context};
use vache_lib::codes::*;
use vache_lib::Arena;

use super::*;

fn test<'ctx>(p: impl Into<Program<'ctx>>) -> Result<()> {
    let arena = Arena::new();
    let config = Config {
        input: "",
        ..default()
    };
    let mut context = Context::new(config, &arena);
    if let Err(err) = vache_lib::typecheck(&mut context, p.into()) {
        context
            .reporter
            .display()
            .expect("Could not print typer errors");
        bail!(err);
    }
    Ok(())
}
