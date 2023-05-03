//! Toy Stratum language compiler.

use vache_lib::{check, interp, mir};

fn main() {
    let checked = check(vache_lib::examples::borrows());
    let mir = mir(checked);
    println!("{:#?}", mir);
    println!("Result: {}", interp(mir));
}
