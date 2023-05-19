//! Toy Stratum language compiler.

use vache_lib::{borrow_check, check, mir, run};

fn main() {
    let mir = borrow_check(mir(check(vache_lib::examples::while_loop())));
    println!("{:?}", mir);
    run(mir, "while", &std::env::current_dir().unwrap()).expect("error");
}
