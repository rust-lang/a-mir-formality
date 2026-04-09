#[test]
fn trait_with_valid_fn() {
    a_mir_formality::assert_ok!(
        [
            crate core {
                trait A {
                    fn a() -> ();
                }
            }
        ]
    );
}

#[test]
fn trait_with_valid_associated_type() {
    a_mir_formality::assert_ok!(
        [
            crate core {
                trait A {
                    type Assoc : [];
                }
            }
        ]
    );
}
