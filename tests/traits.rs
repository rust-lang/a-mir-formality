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
    .skip_execute()
    .rustc_ok()
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
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
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
    .rustc_err(expect_test::expect![[r#"
        error[E0277]: the trait bound `u32: B` is not satisfied
          --> lib.rs
           |
        10 |     type Assoc: A<u32>;
           |                 ^^^^^^ the trait `B` is not implemented for `u32`
           |
        help: this trait has no implementations, consider adding one
          --> lib.rs
           |
         7 | pub trait B {}
           | ^^^^^^^^^^^
        note: required by a bound in `A`
          --> lib.rs
           |
         1 | pub trait A<T01>
           |           - required by a bound in this trait
         2 | where
         3 |     T01: B,
           |          ^ required by this bound in `A`

        For more information about this error, try `rustc --explain E0277`.
        error: could not compile `mycore` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ WellFormedTraitRef(A(<!ty_0 as C>::Assoc, u32)), via: A(<!ty_0 as C>::Assoc, u32), assumptions: {C(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: B(u32), via: A(<!ty_0 as C>::Assoc, u32), assumptions: {C(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
