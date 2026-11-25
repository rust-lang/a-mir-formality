use a_mir_formality::{crates, FormalityTest};

#[test]
fn mixed_bitwise_precedence_in_call_arguments() {
    FormalityTest::new(crates![crate Foo {
        fn identity(x: u32) -> u32 { return x; }
        fn observed() -> u32 { println!(7_u32); return 7_u32; }
        fn main() -> () {
            println!(identity(8_u32 | 6_u32 ^ 3_u32 & 1_u32));
            println!(identity((8_u32 | 6_u32 ^ 3_u32) & 1_u32));
            println!(identity(0_u32 & observed()));
        }
    }])
    .rustc_ok()
    .expect_output("15\n1\n7\n0\n")
    .ok();
}

#[test]
fn compound_and_field_reads_after_rhs() {
    FormalityTest::new(crates![crate Foo {
        struct Pair { left: u32, right: u32 }
        fn update<'a>(p: &'a mut Pair) -> u32 {
            (*p).left = 10_u32;
            return 6_u32;
        }
        fn main() -> () {
            let p: Pair = Pair { left: 1_u32, right: 16_u32 };
            exists<'a> {
                let r: &'a mut Pair = &'a mut p;
                p.left &= update::<'a>(r);
            }
            println!(p.left);
            println!(p.right);
        }
    }])
    .rustc_ok()
    .expect_output("2\n16\n")
    .borrowck_ok();
}
