//! Toy Stratum language compiler.

use vache_lib::{borrow_check, check, mir, run};

fn main() {
    let mut checked = check(vache_lib::examples::assignment_while_borrowed());
    let mir = borrow_check(mir(&mut checked));
    println!("{mir:?}");
    let res = run(checked, "binary", &std::env::current_dir().unwrap()).expect("error");
    println!("{}", res);
}
