use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![
            term("trait Copy<ty Self> where {}"),
            term("trait Magic<ty Self> where {Copy(Self)}"),
        ],
        impl_decls: vec![
            term("impl<ty T> Magic(T) where {Magic(T)}"),
            term("impl Copy(u32) where {}"),
        ],
        ..Decls::empty()
    }
}

#[test]
fn all_t_not_magic() {
    let constraints = test_prove(decls(), term("{} => {for<ty T> Magic(T)}"));
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn all_t_not_copy() {
    let constraints = test_prove(decls(), term("{} => {for<ty T> Copy(T)}"));
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}
