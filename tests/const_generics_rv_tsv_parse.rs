use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

#[test]
fn parse_minirust_22() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<const N> where type_of_const N is usize {}
        impl Trait<usize(22)> for u32 {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}
