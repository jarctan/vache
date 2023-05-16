//! Toy Stratum language compiler.

use vache_lib::{borrow_check, check, compile, mir};

fn main() {
    let checked = check(vache_lib::examples::separate_index_for_array());
    let mir = mir(checked);
    let mir = borrow_check(mir);
    println!("MIR: {:#?}", mir);
    println!("Output: {}", compile(mir));
}
