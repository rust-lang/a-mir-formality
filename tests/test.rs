use rustc_formality;
use std::fs::File;
use std::io::{Read, Write};
use std::process::Command;

#[test]
fn test_issue25860() {
    let output = rustc_formality::run("tests/input/issue25860.rs");
    if let Ok(mut file) = File::open("tests/input/issue25860.rkt") {
        let mut compare_output = String::new();
        file.read_to_string(&mut compare_output).unwrap();
        assert_eq!(output, compare_output);
    } else {
        let mut file = File::create("tests/input/issue25860.rkt").unwrap();
        write!(file, "{output}").unwrap();
    }

    let status = Command::new("racket")
        .args([
            "-l",
            "errortrace",
            "-l",
            "racket/base",
            "-e",
            "(require (submod \"tests/input/issue25860.rkt\" test))",
        ])
        .status()
        .unwrap();

    assert!(status.success());
}
