use a_mir_formality::{crates, FormalityTest};

#[test]
fn last_dereference_read_ends_mutable_loan() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let x: u32 = 4_u32;
            exists<'a> {
                let r: &'a mut u32 = &'a mut x;
                println!(*r | x);
            }
        }
    }])
    .rustc_ok()
    .expect_output("4\n")
    .borrowck_ok();
}

#[test]
fn compound_or_field_destination_reads_after_rhs() {
    FormalityTest::new(crates![crate Foo {
        struct Pair { left: u32, right: u32 }
        fn update<'a>(p: &'a mut Pair) -> u32 {
            (*p).left = 8_u32;
            return 1_u32;
        }
        fn main() -> () {
            let p: Pair = Pair { left: 2_u32, right: 16_u32 };
            exists<'a> {
                let r: &'a mut Pair = &'a mut p;
                p.left |= update::<'a>(r);
            }
            println!(p.left);
            println!(p.right);
        }
    }])
    .rustc_ok()
    .expect_output("9\n16\n")
    .borrowck_ok();
}

#[test]
fn or_in_call_arguments_and_compound_high_bits() {
    FormalityTest::new(crates![crate Foo {
        fn identity(x: u128) -> u128 { return x; }
        fn main() -> () {
            let x: u128 = 170141183460469231731687303715884105728_u128;
            x |= identity(1_u128 | 2_u128 ^ 3_u128);
            println!(x);
            println!(identity((1_u128 | 2_u128) ^ 3_u128));
        }
    }])
    .rustc_ok()
    .expect_output("170141183460469231731687303715884105729\n0\n")
    .ok();
}
