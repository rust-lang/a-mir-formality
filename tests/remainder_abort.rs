use a_mir_formality::crates;
use minirust_rs::prelude::TerminationInfo;

// The ordinary harness panics for both abort and UB; inspect the machine's
// termination reason for these real programs so the distinction is tested.
fn assert_aborts(program: &str) {
    let _proof = a_mir_formality::test_program_ok(program).unwrap();
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(ok, "{stderr}");
    let crates = formality_rust::rust::try_term(program).unwrap();
    let code = formality_rust::codegen::codegen_program(&crates).unwrap();
    type Mem = minirust_rs::mem::BasicMemory<minirust_rs::prelude::x86_64>;
    let mut machine = minirust_rs::lang::Machine::<Mem>::new(
        code,
        libspecr::DynWrite::new(DiscardOutput),
        libspecr::DynWrite::new(DiscardOutput),
    )
    .get_internal()
    .unwrap();
    for _ in 0..1000 {
        if let Err(reason) = machine.step().get_internal() {
            assert!(
                matches!(reason, TerminationInfo::Abort),
                "expected abort, got {reason:?}"
            );
            return;
        }
    }
    panic!("program did not terminate within 1000 steps");
}

#[test]
fn runtime_remainder_zero_aborts() {
    assert_aborts(&crates![crate Foo {
        fn rem(x: i32, y: i32) -> i32 { return x % y; }
        fn main() -> () { println!(rem(7_i32, 0_i32)); }
    }]);
}

#[test]
fn runtime_remainder_overflow_aborts() {
    assert_aborts(&crates![crate Foo {
        fn rem(x: i32, y: i32) -> i32 { return x % y; }
        fn main() -> () { println!(rem(-2147483648_i32, -1_i32)); }
    }]);
}

#[test]
fn runtime_compound_remainder_zero_aborts() {
    assert_aborts(&crates![crate Foo {
        fn rem(x: i32, y: i32) -> i32 { x %= y; return x; }
        fn main() -> () { println!(rem(7_i32, 0_i32)); }
    }]);
}

#[test]
fn runtime_compound_remainder_overflow_aborts() {
    assert_aborts(&crates![crate Foo {
        fn rem(x: i32, y: i32) -> i32 { x %= y; return x; }
        fn main() -> () { println!(rem(-2147483648_i32, -1_i32)); }
    }]);
}

#[test]
fn runtime_unsigned_remainder_zero_aborts() {
    assert_aborts(&crates![crate Foo {
        fn rem(x: u8, y: u8) -> u8 { return x % y; }
        fn main() -> () { println!(rem(7_u8, 0_u8)); }
    }]);
}

#[test]
fn runtime_i8_remainder_overflow_aborts() {
    assert_aborts(&crates![crate Foo {
        fn rem(x: i8, y: i8) -> i8 { return x % y; }
        fn main() -> () { println!(rem(-128_i8, -1_i8)); }
    }]);
}

#[test]
fn runtime_compound_remainder_checks_destination_after_rhs() {
    assert_aborts(&crates![crate Foo {
        struct Pair { left: i32, right: i32 }
        fn update<'a>(p: &'a mut Pair) -> i32 {
            (*p).left = -2147483648_i32;
            return -1_i32;
        }
        fn main() -> () {
            let p: Pair = Pair { left: 7_i32, right: 16_i32 };
            exists<'a> {
                let r: &'a mut Pair = &'a mut p;
                p.left %= update::<'a>(r);
            }
            println!(p.left);
            println!(p.right);
        }
    }]);
}

struct DiscardOutput;
impl std::io::Write for DiscardOutput {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        Ok(bytes.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
impl libspecr::hidden::GcCompat for DiscardOutput {
    fn points_to(&self, _: &mut std::collections::HashSet<usize>) {}
}
