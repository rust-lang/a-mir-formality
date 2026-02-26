use formality_core::test;

#[test]
fn parse_minirust_22() {
    crate::assert_ok!(
        [
            crate Foo {
                trait Trait<const N> where type_of_const N is usize {}
                impl Trait<
                   const (usize) minirust {
                        exists<> {
                            bb1: {
                                statements {
                                    local(_return) = constant(22: usize);
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
