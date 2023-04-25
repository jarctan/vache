use super::*;

mod is_even;

fn test(p: Program, output: impl AsRef<str>) {
    assert_eq!(
        crate::interp(crate::check(p)),
        output.as_ref(),
        "Output mismatch"
    );
}
