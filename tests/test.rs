use rustc_formality::{self, OutputFormat};
use std::fs::File;
use std::io::{Read, Write};

fn generate_and_run(input_path: &str, output_path: &str, expect_failure: bool) {
    let output =
        rustc_formality::run_rustc(input_path, OutputFormat::TestModule, expect_failure).unwrap();

    if let Ok(mut file) = File::open(output_path) {
        let mut compare_output = String::new();
        file.read_to_string(&mut compare_output).unwrap();
        assert_eq!(output, compare_output);
    } else {
        let mut file = File::create(output_path).unwrap();
        write!(file, "{output}").unwrap();
    }

    let racket_expr = format!("(require (submod \"{output_path}\" test))");
    assert!(rustc_formality::run_racket(&racket_expr).unwrap());
}

#[test]
fn test_issue25860() {
    generate_and_run(
        "tests/input/issue25860.rs",
        "tests/input/issue25860.rkt",
        true,
    );
}
