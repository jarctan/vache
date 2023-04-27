//! Toy Stratum language compiler.

use vache_lib::{check, compile};

fn main() {
    println!(
        "{}",
        compile(check(vache_lib::examples::struct_instantiation()))
    );
}
