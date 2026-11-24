//! Local review probes for scalar equality.
use a_mir_formality::{crates, FormalityTest};

#[test]
fn equality_128_bit_high_bits() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(170141183460469231731687303715884105728_u128 == 0_u128);
            println!(340282366920938463463374607431768211455_u128 == 340282366920938463463374607431768211455_u128);
            println!(-170141183460469231731687303715884105728_i128 == 0_i128);
        }
    }]).rustc_ok().expect_output("false\ntrue\nfalse\n").ok();
}

#[test]
fn equality_parenthesized_grouping_and_xor() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(true == (1_u32 == 1_u32));
            println!((true == false) == false);
            println!(5_u32 == 6_u32 ^ 3_u32);
            let b: bool = false;
            b = 2_u32 + 3_u32 == 5_u32;
            println!(b);
        }
    }])
    .rustc_ok()
    .expect_output("true\ntrue\ntrue\ntrue\n")
    .ok();
}

#[test]
fn equality_repeated_copy_dereference() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let x: u32 = 1_u32;
            exists<'a> {
                let r: &'a u32 = &'a x;
                println!(*r == *r);
            }
        }
    }])
    .rustc_ok()
    .expect_output("true\n")
    .ok();
}

#[test]
fn equality_read_after_loan_executes() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let x: u32 = 1_u32;
            exists<'a> {
                let r: &'a mut u32 = &'a mut x;
                println!(*r == x);
            }
        }
    }])
    .rustc_ok()
    .expect_output("true\n")
    .borrowck_ok();
}
