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

#[test]
fn trait_with_ill_formed_where_clause() {
    a_mir_formality::assert_err!(
        [
            crate core {
                trait A<T> where T: B {}
                trait B {}

                trait C {
                    type Assoc : [ A<u32> ];
                }
            }
        ]
        expect_test::expect![[r#"
            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    );
}
