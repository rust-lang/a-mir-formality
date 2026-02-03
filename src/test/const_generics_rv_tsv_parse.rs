use formality_core::test;

#[test]
fn parse_minirust_22() {
    crate::assert_ok!(
        [
            crate Foo {
                trait Trait<const N> where type_of_const N is usize {}
                impl Trait<
                   const minirust() -> v0 {
                        let v0: usize;

                        exists<> {
                            bb1: {
                                statements {
                                    local(v0) = constant(22: usize);
                                }
                                return;
                            }
                        }
                    }
                > for u32 {}
            }
        ]
    )
}
