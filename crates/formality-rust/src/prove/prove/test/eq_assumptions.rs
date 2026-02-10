use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::prove::prove::decls::Decls;

use crate::prove::prove::test_util::test_prove;

#[test]
fn test_a() {
    test_prove(
        Decls::empty(),
        term("{} => {for<ty T, ty U> if {T = u32, U = Vec<T>} U = Vec<u32>}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_b() {
    test_prove(
        Decls::empty(),
        term("exists<ty A> {} => {for<ty T, ty U> if {T = u32, U = Vec<T>} A = U}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [?ty_2, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<u32>, ?ty_2 => u32} }}"]);
}

#[test]
fn test_normalize_assoc_ty() {
    test_prove(
        Decls::empty(),
        term("{} => {for<ty T> if { <T as Iterator>::Item = u32 } <T as Iterator>::Item = u32}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_normalize_assoc_ty_existential0() {
    test_prove(
        Decls::empty(),
        term("exists<ty A> {} => {for<ty T> if { <T as Iterator>::Item = u32 } <A as Iterator>::Item = u32}"),
    ).assert_err(
    expect![[r#"
        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_1)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_1)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_0)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluted to false: `env.universe(p) < env.universe(v)`

        the rule "normalize-via-impl" at (prove_normalize.rs) failed because
          expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`"#]]);
}

#[test]
fn test_normalize_assoc_ty_existential1() {
    test_prove(
        Decls::empty(),
        term(
            "\
            forall<ty T> \
            exists<ty A> \
            { <T as Iterator>::Item = u32 } => { <A as Iterator>::Item = u32 }",
        ),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);
}
