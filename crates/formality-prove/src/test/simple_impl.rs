use expect_test::expect;
use formality_macros::test;
use formality_types::grammar::Wc;
use formality_types::rust::term;

use crate::{decls::Decls, prove::prove};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![term("trait Debug<ty Self> where {}")],
        impl_decls: vec![
            term("impl<ty T> Debug(Vec<T>) where {Debug(T)}"),
            term("impl Debug(u32) where {}"),
        ],
        ..Decls::empty()
    }
}

#[test]
fn vec_u32_debug() {
    let goal: Wc = term("Debug(Vec<u32>)");
    prove(decls(), (), (), goal).assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} },
        }
    "#]]);
}

#[test]
fn vec_vec_u32_debug() {
    let goal: Wc = term("Debug(Vec<Vec<u32>>)");
    prove(decls(), (), (), goal).assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} },
        }
    "#]]);
}
