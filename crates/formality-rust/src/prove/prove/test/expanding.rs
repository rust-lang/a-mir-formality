use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::{test_util::test_prove, Decls};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        max_size: 10,
        program: Decls::program_from_items(vec![
            term("trait Debug where {}"),
            term("impl<ty T> Debug for Vec<T> where T : Debug {}"),
        ]),
        ..Decls::empty()
    }
}

/// There is no U that is equal to all T.
#[test]
fn expanding() {
    test_prove(decls(), term("exists<ty T> {} => {Debug(T)}")).assert_ok(expect!["{Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: false, substitution: {} }}"]);
}
