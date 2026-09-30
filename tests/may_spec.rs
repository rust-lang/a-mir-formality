//! Branch specialization (`#![feature(branch_specialization)]`): `if impls`
//! and `may_spec` bounds. See the book chapter for the rules. Every behavior
//! test runs its program under every mode (`FormalityTest::spec_modes`).
//!
//! (`Tag<'a>` structs carry a lifetime without a borrow because codegen
//! cannot yet execute programs with live loans.)

#![allow(non_snake_case)]

use a_mir_formality::{crates, FormalityTest};
use formality_macros::test;

// ---------------------------------------------------------------------------
// The feature gate, and the form and well-formedness of the bounds
// ---------------------------------------------------------------------------

#[test]
fn if_impls_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn main() -> () {
            if impls u32: Bar { println!(1_u32); }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// The feature gate is required wherever `may_spec` appears: on impls...
#[test]
fn may_spec_requires_feature_gate_on_impl() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Baz {}
        impl<T> Baz for T where may_spec(T: Bar) {}
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// ...and on ADTs...
#[test]
fn may_spec_requires_feature_gate_on_struct() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        struct S<T> where may_spec(T: Bar) {}
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate_under_for_all() {
    FormalityTest::new(crates![crate foo {
        trait Tr<'a> {}
        fn spec<T>() -> () where for<'a> may_spec(T: Tr<'a>) { }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate_on_associated_type() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Tr {
            type Out : [] where may_spec(Self: Bar);
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// ...and for an `if impls` nested inside other statements.
#[test]
fn if_impls_nested_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn main() -> () {
            'a: loop {
                if impls u32: Bar { println!(1_u32); } else { println!(2_u32); }
                break 'a;
            }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*branch_specialization_enabled`"#]])
}

/// Only a trait bound parses as the bound.
#[test]
fn may_spec_requires_trait_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        fn spec<'a, T>() -> () where may_spec(T: 'a) { }
    }])
    .err(expect_test::expect![[r#"
        × TraitId expected
           ╭─[1:1]
         1 │ [crate foo
           · ▲▲▲
           · ││╰── while parsing Crate
           · │╰── while parsing Vec
           · ╰── while parsing Crates
         2 │ {
         3 │     #![feature(branch_specialization)] fn spec<'a, T>() -> () where
           ·                                        ▲▲     ▲      ▲
           ·                                        ││     │      ╰── while parsing FnBoundData
           ·                                        ││     ╰── while parsing Binder
           ·                                        │╰── while parsing Fn
           ·                                        ╰── while parsing CrateItem
         4 │     may_spec(T: 'a) {}
           ·     ▲           ▲▲
           ·     │           │╰── TraitId expected
           ·     │           ╰── while parsing TraitId
           ·     ╰── while parsing WhereClause
         5 │ }]
           ╰────"#]])
}

#[test]
fn if_impls_requires_trait_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        fn spec<'a, T>() -> () {
            if impls T: 'a { } else { }
        }
    }])
    .err(expect_test::expect![[r#"
        × TraitId expected
           ╭─[1:1]
         1 │ [crate foo
           · ▲▲▲
           · ││╰── while parsing Crate
           · │╰── while parsing Vec
           · ╰── while parsing Crates
         2 │ {
         3 │     #![feature(branch_specialization)] fn spec<'a, T>() -> ()
           ·                                        ▲▲     ▲      ▲
           ·                                        ││     │      ╰── while parsing FnBoundData
           ·                                        ││     ╰── while parsing Binder
           ·                                        │╰── while parsing Fn
           ·                                        ╰── while parsing CrateItem
         4 │     { if impls T: 'a {} else {} }
           ·     ▲▲▲▲       ▲  ▲▲
           ·     ││││       │  │╰── TraitId expected
           ·     ││││       │  ╰── while parsing TraitId
           ·     ││││       ╰── while parsing IfImplsBound
           ·     │││╰── while parsing Stmt
           ·     ││╰── while parsing Block
           ·     │╰── while parsing FnBody
           ·     ╰── while parsing MaybeFnBody
         5 │ }]
           ╰────"#]])
}

/// `may_spec` bounds: on a trait bound; `for<..>` outside, as on any
/// where-clause.
#[test]
fn may_spec_accepted() {
    FormalityTest::new(crates![crate foo {
        trait Bar<'a> {}
        fn spec<T>() -> () where may_spec(T: Bar<'static>), for<'a> may_spec(T: Bar<'a>) { }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// `may_spec(T: Sub)` does not assume `T: Sub`, so `T: Super` is not
/// required...
#[test]
fn may_spec_does_not_require_supertraits() {
    FormalityTest::new(crates![crate foo {
        trait Super {}
        trait Sub where Self: Super {}
        fn spec<T>() -> () where may_spec(T: Sub) { }
    }])
    .skip_execute()
    .spec_modes(expect_test::expect![[r#"
        strict: ok
    "#]])
}

/// ...but the parameters of the bound must still be well-formed.
#[test]
fn may_spec_requires_well_formed_parameters() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        trait Baz {}
        struct S<T> where T: Bar {}
        fn spec<T>() -> () where may_spec(S<T>: Baz) { }
    }])
    .spec_modes(expect_test::expect![[r#"
        strict: err:
            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(S<!ty_0>), via: @ may_spec(Baz(S<!ty_0>)), assumptions: {@ may_spec(Baz(S<!ty_0>))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Bar(!ty_0), via: @ may_spec(Baz(S<!ty_0>)), assumptions: {@ may_spec(Baz(S<!ty_0>))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`
    "#]])
}
