use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::{test_util::test_prove, Decls};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        max_size: 10,
        trait_decls: vec![term("trait Debug<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Debug(Vec<T>) where {Debug(T)}")],
        ..Decls::empty()
    }
}

/// There is no U that is equal to all T.
#[test]
fn expanding() {
    test_prove(decls(), term("exists<ty T> {} => {Debug(T)}")).assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [?ty_0], bias: Soundness }, known_true: false, substitution: {} },
        }
    "#]]);
}
