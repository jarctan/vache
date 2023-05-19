//! Toy Stratum language compiler.

use vache_lib::execute;

fn main() {
    execute(
        vache_lib::examples::while_loop(),
        "while",
        &std::env::current_dir().unwrap(),
    )
    .expect("error");
}
