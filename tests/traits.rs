use a_mir_formality::{crates, FormalityTest};

#[test]
fn trait_with_valid_fn() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                fn a() -> ();
            }
        }
    ])
    .ok();
}

#[test]
fn trait_with_valid_associated_type() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                type Assoc : [];
            }
        }
    ])
    .ok();
}

#[test]
#[ignore = "ensures bounds WF check not yet implemented, see FIXME(#228)"]
fn trait_with_ill_formed_where_clause() {
    FormalityTest::new(crates![
        crate core {
            trait A<T> where T: B {}
            trait B {}
            trait C {
                type Assoc : [ A<u32> ];
            }
        }
    ])
    .err(expect_test::expect![[r#"
            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
