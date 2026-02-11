use crate::grammar::Wc;
use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::{decls::Decls, prove::prove};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        program: Decls::program_from_items(vec![
            term("trait Debug where {}"),
            term("impl<ty T> Debug for Vec<T> where T : Debug {}"),
            term("impl Debug for u32 {}"),
        ]),
        ..Decls::empty()
    }
}

#[test]
fn vec_u32_debug() {
    let goal: Wc = term("Debug(Vec<u32>)");
    prove(decls(), (), (), goal).assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn vec_vec_u32_debug() {
    let goal: Wc = term("Debug(Vec<Vec<u32>>)");
    prove(decls(), (), (), goal).assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}
