use crate::rust::term;
use expect_test::expect;
use formality_macros::test;
use std::sync::Arc;

use crate::prove::decls::Program;

use crate::prove::test_util::{test_prove, test_prove_pending_outlives};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Foo where {}"),
            term("impl<T> Foo for Vec<T> {}"),
        ])),
        ..Program::empty()
    }
}

/// Test that `exists<T> Foo(U)` yields `U = Vec<X>` for some fresh `X`
#[test]
fn exists_u_for_t() {
    test_prove(decls(), term("exists<U> {} => {Foo(U)}")).assert_ok(expect!["{Constraints { env: Env { variables: [?ty_2, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<?ty_2>} }}"]);
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
