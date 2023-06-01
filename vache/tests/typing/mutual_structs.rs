//! Checking that we accept two mutually dependant struct declaration (A
//! mentions B and vice versa).

use super::*;

#[test]
fn mutual_structs() {
    test(Program::new(
        vec![
            Struct {
                name: "Person",
                fields: vec![("name", StrT), ("age", IntT), ("house", StructT("House"))]
                    .into_iter()
                    .collect(),
            },
            Struct {
                name: "House",
                fields: vec![
                    ("name", StrT),
                    ("owner", StructT("Person")),
                    ("built", IntT),
                ]
                .into_iter()
                .collect(),
            },
        ],
        vec![],
    ));
}
