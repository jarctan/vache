//! Toy Stratum language compiler.

use vache_lib::{borrow_check, check, interp, mir};

fn main() {
    let checked = check(vache_lib::examples::borrows());
    let mir = mir(checked);
    println!("{:#?}", mir);
    borrow_check(&mir);
    println!("Output: {}", interp(mir));
}
