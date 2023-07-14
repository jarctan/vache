//! Checking that we accept two mutually dependant struct declaration (A
//! mentions B and vice versa).

use super::*;

#[test]
fn mutual_structs() -> Result<()> {
    test(Program::new(
        vec![
            struct_def(
                "Person",
                [("name", strT()), ("age", intT()), ("house", varT("House"))],
            ),
            struct_def(
                "House",
                [
                    ("name", strT()),
                    ("owner", varT("Person")),
                    ("built", intT()),
                ],
            ),
        ],
        default(),
        vec![Fun {
            name: "main",
            ..default()
        }],
    ))
}
