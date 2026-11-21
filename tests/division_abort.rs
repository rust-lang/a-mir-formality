//! Invalid integer division must abort, not trigger MiniRust undefined behavior.

use a_mir_formality::{crates, FormalityTest};
use minirust_rs::prelude::TerminationInfo;

/// Run the generated program and distinguish a defined abort from UB.
#[track_caller]
fn assert_aborts(input: &str) {
    // Check typing and rustc compilation first. Execution is performed below
    // so its termination reason can be inspected rather than turned into a panic.
    FormalityTest::new(input).rustc_ok().skip_execute().ok();

    let crates = formality_rust::rust::try_term(input).expect("failed to parse program");
    let program = formality_rust::codegen::codegen_program(&crates).expect("codegen failed");
    type Mem = minirust_rs::mem::BasicMemory<minirust_rs::prelude::x86_64>;
    let mut machine = minirust_rs::lang::Machine::<Mem>::new(
        program,
        libspecr::DynWrite::new(std::io::stdout()),
        libspecr::DynWrite::new(std::io::stderr()),
    )
    .get_internal()
    .expect("machine creation failed");

    for _ in 0..10_000 {
        match machine.step().get_internal() {
            Ok(()) => {}
            Err(TerminationInfo::Abort) => return,
            Err(other) => panic!("expected a defined abort, got {other:?}"),
        }
    }
    panic!("execution step limit reached before abort");
}

// Use function parameters so rustc checks a runtime division rather than
// rejecting a constant expression with the unconditional_panic lint.
#[test]
fn division_by_zero_aborts() {
    assert_aborts(crates![crate Foo {
        fn divide(lhs: u32, rhs: u32) -> u32 { return lhs / rhs; }
        fn main() -> () { println!(divide(12_u32, 0_u32)); }
    }]);
}

#[test]
fn signed_division_overflow_aborts() {
    assert_aborts(crates![crate Foo {
        fn divide(lhs: i32, rhs: i32) -> i32 { return lhs / rhs; }
        fn main() -> () { println!(divide(-2147483648_i32, -1_i32)); }
    }]);
}

#[test]
fn compound_division_by_zero_aborts() {
    assert_aborts(crates![crate Foo {
        fn divide(lhs: u32, rhs: u32) -> u32 {
            lhs /= rhs;
            return lhs;
        }
        fn main() -> () { println!(divide(12_u32, 0_u32)); }
    }]);
}

#[test]
fn signed_compound_division_overflow_aborts() {
    assert_aborts(crates![crate Foo {
        fn divide(lhs: i32, rhs: i32) -> i32 {
            lhs /= rhs;
            return lhs;
        }
        fn main() -> () { println!(divide(-2147483648_i32, -1_i32)); }
    }]);
}
