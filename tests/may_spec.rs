//! Branch specialization (`#![feature(branch_specialization)]`): `if impls`
//! and `may_spec` bounds. See the book chapter for the rules.

#![allow(non_snake_case)]

use a_mir_formality::{crates, FormalityTest};
use formality_macros::test;

// ---------------------------------------------------------------------------
// The feature gate and the form of the bounds
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
          condition evaluated to false: `*enabled`"#]])
}

#[test]
fn may_spec_requires_feature_gate() {
    FormalityTest::new(crates![crate foo {
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
    }])
    .err(expect_test::expect![[r#"
        the rule "feature gate" at (specialization.rs) failed because
          condition evaluated to false: `*enabled`"#]])
}

/// With the feature gate, `may_spec` bounds are accepted: on a trait bound,
/// possibly under `for<..>`.
#[test]
fn may_spec_accepted_with_feature_gate() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar<'a> {}
        fn spec<T>() -> () where may_spec(T: Bar<'static>), may_spec(for<'a> T: Bar<'a>) { }
    }])
    .skip_execute()
    .ok()
}

/// `may_spec(T: Sub)` does not assume `T: Sub`, so `T: Super` is not
/// required...
#[test]
fn may_spec_does_not_require_supertraits() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Super {}
        trait Sub where Self: Super {}
        fn spec<T>() -> () where may_spec(T: Sub) { }
    }])
    .skip_execute()
    .ok()
}

/// ...but the parameters of the bound must still be well-formed.
#[test]
fn may_spec_requires_well_formed_parameters() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        trait Baz {}
        struct S<T> where T: Bar {}
        fn spec<T>() -> () where may_spec(S<T>: Baz) { }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ wf(S<!ty_0>), via: @ may_spec(S<!ty_0> : Baz), assumptions: {@ may_spec(S<!ty_0> : Baz)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Bar(!ty_0), via: @ may_spec(S<!ty_0> : Baz), assumptions: {@ may_spec(S<!ty_0> : Baz)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
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
          condition evaluated to false: `*enabled`"#]])
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
          condition evaluated to false: `*enabled`"#]])
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
          condition evaluated to false: `*enabled`"#]])
}

/// Only trait bounds (possibly under `for<..>`) parse as the bound.
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
           ·     ▲        ▲  ▲▲
           ·     │        │  │╰── TraitId expected
           ·     │        │  ╰── while parsing TraitId
           ·     │        ╰── while parsing MaySpecBound
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
           ·     ││││       ╰── while parsing MaySpecBound
           ·     │││╰── while parsing Stmt
           ·     ││╰── while parsing Block
           ·     │╰── while parsing FnBody
           ·     ╰── while parsing MaybeFnBody
         5 │ }]
           ╰────"#]])
}

// ---------------------------------------------------------------------------
// `may_spec` at call sites: the caller decides
// ---------------------------------------------------------------------------

/// Concrete callers decide on the spot: `u32: Bar` holds; `i32: Bar` is
/// closed and unprovable.
#[test]
fn may_spec_concrete_caller_decides() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        impl Bar for u32 {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .skip_execute()
    .ok()
}

/// A generic caller cannot decide `T: Bar` and so may not call `spec::<T>`...
#[test]
fn may_spec_generic_caller_without_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () {
            spec::<T>();
        }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/may_spec.rs:30:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

/// ...unless it knows `T: Bar`...
#[test]
fn may_spec_generic_caller_with_positive_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () where T: Bar {
            spec::<T>();
        }
    }])
    .skip_execute()
    .ok()
}

/// ...or defers with its own `may_spec`.
#[test]
fn may_spec_generic_caller_with_may_spec_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () where may_spec(T: Bar) {
            spec::<T>();
        }
    }])
    .skip_execute()
    .ok()
}

/// `may_spec(T: Sub)` does not decide `T: Super` (see the book, "What a
/// `may_spec` bound decides"): `caller` must declare `may_spec(T: Super)`.
#[test]
fn may_spec_subtrait_bound_does_not_decide_supertrait() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Super {}
        trait Sub where Self: Super {}
        fn spec<T>() -> () where may_spec(T: Super) { }
        fn caller<T>() -> () where may_spec(T: Sub) {
            spec::<T>();
        }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(!ty_0 : Super), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "same bound" at (may_spec.rs) failed because
          condition evaluated to false: `bounds.contains(&goal)`
            bounds = [Sub(!ty_0)]
            &goal = Super(!ty_0)

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Super(!ty_0), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(!ty_0 : Sub), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(!ty_0 : Sub)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]])
}

/// Nor does `may_spec(T: Super)` decide `T: Sub`.
#[test]
fn may_spec_supertrait_bound_does_not_decide_subtrait() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Super {}
        trait Sub where Self: Super {}
        fn spec<T>() -> () where may_spec(T: Sub) { }
        fn caller<T>() -> () where may_spec(T: Super) {
            spec::<T>();
        }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ may_spec(!ty_0 : Sub), via: @ may_spec(!ty_0 : Super), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "same bound" at (may_spec.rs) failed because
          condition evaluated to false: `bounds.contains(&goal)`
            bounds = [Super(!ty_0)]
            &goal = Sub(!ty_0)

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: @ may_spec(!ty_0 : Super), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Sub(!ty_0), via: Super(?ty_1), assumptions: {@ may_spec(!ty_0 : Super)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]])
}

/// With two `may_spec` bounds in scope, each decides the bound it names.
#[test]
fn may_spec_two_bounds() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        trait Baz {}
        fn spec<T>() -> () where may_spec(T: Bar), may_spec(T: Baz) { }
        fn caller<T>() -> () where may_spec(T: Baz), may_spec(T: Bar) {
            spec::<T>();
        }
    }])
    .skip_execute()
    .ok()
}

/// A bound over a generic type is never decided by a failed search. We could,
/// in theory, be certain here because this impl cannot be added downstream. But,
/// for now we require that caller must declare `may_spec(Wrapper<T>: Bar)` itself.
#[test]
fn may_spec_generic_wrapper_is_undecided() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        struct Wrapper<T> { value: T }
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () {
            spec::<Wrapper<T>>();
        }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/may_spec.rs:30:1: no applicable rules for decide_by_bound { goal: Bar(Wrapper<!ty_0>), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

/// A negative impl is never consulted: `T: Bar` stays undecided, since a
/// downstream type may implement `Bar`.
#[test]
fn may_spec_negative_impl_does_not_decide_generic() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        #![feature(negative_impls)]
        trait Bar {}
        struct Wrapper<T> { value: T }
        impl<T> !Bar for Wrapper<T> {}
        fn spec<T>() -> () where may_spec(T: Bar) { }
        fn caller<T>() -> () {
            spec::<Wrapper<T>>();
        }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/may_spec.rs:30:1: no applicable rules for decide_by_bound { goal: Bar(Wrapper<!ty_0>), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

/// A downstream crate decides using its own types and impls.
#[test]
fn may_spec_decided_downstream() {
    FormalityTest::new(crates![
        crate upstream {
            #![feature(branch_specialization)]
            trait Bar {}
            fn spec<T>() -> () where may_spec(T: Bar) { }
        },
        crate downstream {
            struct Yes {}
            struct No {}
            impl Bar for Yes {}
            fn main() -> () {
                spec::<Yes>();
                spec::<No>();
            }
        }
    ])
    .skip_execute()
    .ok()
}

// ---------------------------------------------------------------------------
// `if impls` on closed types: decided locally
// ---------------------------------------------------------------------------

#[test]
fn if_impls_concrete_holds() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        impl Bar for u32 {}
        fn main() -> () {
            if impls u32: Bar { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .expect_output("1\n")
    .ok()
}

#[test]
fn if_impls_concrete_does_not_hold() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        impl Bar for u32 {}
        fn main() -> () {
            if impls i32: Bar { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .expect_output("2\n")
    .ok()
}

/// A local type with no impl: the bound is closed and not provable, so it
/// does not hold and the else branch is taken.
#[test]
fn if_impls_local_type_does_not_hold() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        struct Local {}
        fn main() -> () {
            if impls Local: Bar { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .expect_output("2\n")
    .ok()
}

// ---------------------------------------------------------------------------
// `if impls` on generics: decided by the callers, evaluated at codegen
// ---------------------------------------------------------------------------

/// Without `may_spec(T: Bar)`, `T: Bar` can be neither proven nor refuted
/// inside the function, so this is an error.
#[test]
fn if_impls_generic_without_may_spec() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        fn spec<T>() -> () {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/may_spec.rs:30:1: no applicable rules for decide_by_bound { goal: Bar(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

/// With `may_spec(T: Bar)`, each caller decides, and each monomorphization
/// takes its own branch.
#[test]
fn if_impls_generic_with_may_spec() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        impl Bar for u32 {}
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .expect_output("1\n2\n")
    .ok()
}

/// A chain of generic callers: `caller` defers `T: Bar` to its own callers.
#[test]
fn if_impls_decided_through_generic_caller() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        impl Bar for u32 {}
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn caller<T>() -> () where may_spec(T: Bar) {
            spec::<T>();
        }
        fn main() -> () {
            caller::<i32>();
            caller::<u32>();
        }
    }])
    .expect_output("2\n1\n")
    .ok()
}

/// A downstream crate decides using its own types and impls.
#[test]
fn if_impls_decided_downstream() {
    FormalityTest::new(crates![
        crate upstream {
            #![feature(branch_specialization)]
            trait Bar {}
            fn spec<T>() -> () where may_spec(T: Bar) {
                if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
            }
        },
        crate downstream {
            struct Yes {}
            struct No {}
            impl Bar for Yes {}
            fn main() -> () {
                spec::<Yes>();
                spec::<No>();
            }
        }
    ])
    .expect_output("1\n2\n")
    .ok()
}

// ---------------------------------------------------------------------------
// Assumptions inside the branches
// ---------------------------------------------------------------------------

/// Inside the then-branch, `T: Bar` is assumed, so a function requiring it
/// may be called; in the else-branch it may not.
#[test]
fn if_impls_then_branch_assumes_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        fn needs_bar<T>() -> () where T: Bar { }
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { needs_bar::<T>(); }
        }
    }])
    .skip_execute()
    .ok()
}

#[test]
fn if_impls_else_branch_does_not_assume_bound() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        fn needs_bar<T>() -> () where T: Bar { }
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { } else { needs_bar::<T>(); }
        }
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: Bar(!ty_0), via: @ may_spec(!ty_0 : Bar), assumptions: {@ may_spec(!ty_0 : Bar)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

/// The else-branch assumes nothing new, but the `may_spec` bound still
/// decides a nested use there.
#[test]
fn if_impls_else_branch_nested_call() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        trait Bar {}
        impl Bar for u32 {}
        fn inner<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(1_u32); } else { println!(2_u32); }
        }
        fn spec<T>() -> () where may_spec(T: Bar) {
            if impls T: Bar { println!(3_u32); } else { inner::<T>(); }
        }
        fn main() -> () {
            spec::<u32>();
            spec::<i32>();
        }
    }])
    .expect_output("3\n2\n")
    .ok()
}

/// A `'static` in the generic arguments is erased at codegen like any
/// lifetime; the then-branch is still taken, as `main` decided.
#[test]
fn if_impls_static_argument_is_erased_at_codegen() {
    FormalityTest::new(crates![crate foo {
        #![feature(branch_specialization)]
        struct Tag<'a> {}
        trait Static {}
        impl Static for Tag<'static> {}
        fn spec<'a>(t: Tag<'a>) -> () where may_spec(Tag<'a>: Static) {
            if impls Tag<'a>: Static { println!(1_u32); } else { println!(2_u32); }
        }
        fn main() -> () {
            spec::<'static>(Tag::<'static> {});
        }
    }])
    .expect_output("1\n")
    .ok()
}
