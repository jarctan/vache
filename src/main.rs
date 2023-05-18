//! Toy Stratum language compiler.

use vache_lib::{borrow_check, check, compile, mir};

fn main() {
    let checked = check(vache_lib::examples::while_loop2());
    let mir = mir(checked);
    let mir = borrow_check(mir);
    println!("MIR: {:#?}", mir);
    println!("Output: {}", compile(mir));
}
