//! Tests for where-clauses left pending: outlives, when the environment
//! allows it.

use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::decls::Program;

use crate::prove::test_util::test_prove_pending_outlives;

/// A where-clause is pending once, however many proofs follow it.
#[test]
fn pending_once() {
    test_prove_pending_outlives(
        Program::empty(),
        term("exists<'x, 'y, 'z> {} => {'x : 'static, 'y : 'static, 'z : 'static}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [?lt_1, ?lt_2, ?lt_3], bias: Soundness, pending: [?lt_1 : ' static, ?lt_2 : ' static, ?lt_3 : ' static], allow_pending_outlives: true }, known_true: true, substitution: {} }}"]);
}

/// A where-clause pending from one goal is not renamed by the proof of the
/// next: `'x: 'static` does not become `'y: 'static`.
#[test]
fn pending_not_renamed() {
    test_prove_pending_outlives(
        Program::empty(),
        term("exists<'x, 'y, 'z> {} => {'x : 'static, 'y : 'z}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [?lt_1, ?lt_2, ?lt_3], bias: Soundness, pending: [?lt_1 : ' static, ?lt_2 : ?lt_3], allow_pending_outlives: true }, known_true: true, substitution: {} }}"]);
}
