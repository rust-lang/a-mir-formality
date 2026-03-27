use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::{test_util::test_prove, Program};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Program {
    Program {
        max_size: 10,
        crates: Program::program_from_items(vec![
            term("trait Debug where {}"),
            term("impl<T> Debug for Vec<T> where T : Debug {}"),
        ]),
        ..Program::empty()
    }
}

/// There is no U that is equal to all T.
#[test]
fn expanding() {
    test_prove(decls(), term("exists<T> {} => {Debug(T)}")).assert_ok(expect!["{Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: false, substitution: {} }}"]);
}
