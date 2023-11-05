use formality_core::test;

use crate::eg::term;

use super::{Expr, StructDecl, Ty};

#[test]
fn test_struct_decl() {
    let r: StructDecl = term("struct Point { x: integer, y: integer }");
    expect_test::expect![[r#"
        struct Point { x : integer, y : integer }
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_struct_ty_empty_args() {
    let r: Ty = term("Point<>");
    expect_test::expect![[r#"
        Point
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_struct_ty_no_args() {
    let r: Ty = term("Point");
    expect_test::expect![[r#"
        Point
    "#]]
    .assert_debug_eq(&r);
}
#[test]
fn test_vec_int_ty() {
    let r: Ty = term("Vec<integer>");
    expect_test::expect![[r#"
        Vec <integer>
    "#]]
    .assert_debug_eq(&r);
}
