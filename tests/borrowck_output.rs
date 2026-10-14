use a_mir_formality::{crates, FormalityTest};

#[test]
#[should_panic(expected = "program output mismatch")]
fn borrowck_checks_output_expectation() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .expect_output("999\n")
    .borrowck_ok();
}

#[test]
fn borrowck_accepts_matching_output() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .expect_output("1\n")
    .borrowck_ok();
}

#[test]
#[should_panic(expected = "program output mismatch")]
fn ok_checks_output_expectation() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .expect_output("999\n")
    .ok();
}

#[test]
fn skipped_execution_does_not_check_output() {
    let program = crates![crate Foo {
        fn example() -> () {}
    }];

    FormalityTest::new(program)
        .expect_output("999\n")
        .skip_execute()
        .borrowck_ok();

    FormalityTest::new(program)
        .expect_output("999\n")
        .skip_execute()
        .ok();
}
