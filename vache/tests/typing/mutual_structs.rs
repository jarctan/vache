//! Checking that we accept two mutually dependant struct declaration (A
//! mentions B and vice versa).

use super::*;

#[test]
fn mutual_structs() {
    test(Program::new(
        vec![
            Struct {
                name: "Person",
                fields: vec![("name", strT()), ("age", intT()), ("house", varT("House"))]
                    .into_iter()
                    .collect(),
                ..default()
            },
            Struct {
                name: "House",
                fields: vec![
                    ("name", strT()),
                    ("owner", varT("Person")),
                    ("built", intT()),
                ]
                .into_iter()
                .collect(),
                ..default()
            },
        ],
        default(),
        default(),
    ));
}
