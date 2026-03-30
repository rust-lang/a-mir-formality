use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Program;

use crate::prove::prove::test_util::test_prove;

#[test]
fn test_a() {
    test_prove(
        Program::empty(),
        term("{} => {for<T, U> if {T = u32, U = Vec<T>} U = Vec<u32>}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_b() {
    test_prove(
        Program::empty(),
        term("exists<A> {} => {for<T, U> if {T = u32, U = Vec<T>} A = U}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [?ty_2, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<u32>, ?ty_2 => u32} }}"]);
}

#[test]
fn test_normalize_assoc_ty() {
    test_prove(
        Program::empty(),
        term("{} => {for<T> if { <T as Iterator>::Item = u32 } <T as Iterator>::Item = u32}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_normalize_assoc_ty_existential0() {
    test_prove(
        Program::empty(),
        term("exists<A> {} => {for<T> if { <T as Iterator>::Item = u32 } <A as Iterator>::Item = u32}"),
    ).assert_err(
    expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <?ty_0 as Iterator>::Item = u32, via: <!ty_1 as Iterator>::Item = u32, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_1)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_1)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <!ty_0 as Iterator>::Item = <?ty_1 as Iterator>::Item, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = ?ty_1, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: ?ty_1, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: u32 = <?ty_1 as Iterator>::Item, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: ?ty_1 = !ty_0, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: ?ty_1, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`"#]]);
}

#[test]
fn test_normalize_assoc_ty_existential1() {
    test_prove(
        Program::empty(),
        term(
            "\
            forall<T> \
            exists<A> \
            { <T as Iterator>::Item = u32 } => { <A as Iterator>::Item = u32 }",
        ),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);
}
