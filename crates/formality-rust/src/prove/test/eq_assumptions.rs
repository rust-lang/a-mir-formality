use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::decls::Program;

use crate::prove::test_util::test_prove;

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
        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: <?ty_0 as Iterator>::Item = u32, via: <!ty_1 as Iterator>::Item = u32, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        failed at (proven_set.rs) because
          no matching normalized forms"#]]);
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
