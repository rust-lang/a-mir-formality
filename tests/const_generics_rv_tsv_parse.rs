use a_mir_formality::assert_ok;
use formality_core::test;

#[test]
fn parse_minirust_22() {
    assert_ok!(
        [
            crate Foo {
                trait Trait<const N> where type_of_const N is usize {}
                impl Trait<usize(22)> for u32 {}
            }
        ]
    )
}
