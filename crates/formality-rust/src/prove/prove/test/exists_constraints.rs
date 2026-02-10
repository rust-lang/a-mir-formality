use crate::types::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Decls;

use crate::prove::prove::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Foo(Vec<T>) where {}")],
        ..Decls::empty()
    }
}

/// Test that `exists<T> Foo(U)` yields `U = Vec<X>` for some fresh `X`
#[test]
fn exists_u_for_t() {
    test_prove(decls(), term("exists<ty U> {} => {Foo(U)}")).assert_ok(expect!["{Constraints { env: Env { variables: [?ty_2, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<?ty_2>} }}"]);
}
