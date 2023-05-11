//! Toy Stratum language compiler.

use vache_lib::{borrow_check, check, interp, mir};

fn main() {
    let checked = check(vache_lib::examples::borrows());
    let mir = mir(checked);
    let mir = borrow_check(mir);
    println!("{:#?}", mir);
    println!("Output: {}", interp(mir));
}
