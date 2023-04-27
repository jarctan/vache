use super::*;

#[test]
fn mutual_structs() {
    test(Program::new(
        vec![
            Struct {
                name: "Person".to_string(),
                fields: vec![
                    ("name".to_string(), StrT),
                    ("age".to_string(), IntT),
                    ("house".to_string(), StructT("House".to_string())),
                ]
                .into_iter()
                .collect(),
            },
            Struct {
                name: "House".to_string(),
                fields: vec![
                    ("name".to_string(), StrT),
                    ("owner".to_string(), StructT("Person".to_string())),
                    ("built".to_string(), IntT),
                ]
                .into_iter()
                .collect(),
            },
        ],
        vec![],
    ));
}
