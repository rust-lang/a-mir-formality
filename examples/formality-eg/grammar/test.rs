use formality_core::test;

use crate::eg::term;

use super::{Expr, StructDecl, Ty};

#[test]
fn test_struct_decl() {
    let r: StructDecl = term("struct Point { x: integer, y: integer }");
    expect_test::expect![[r#"
        StructDecl {
            id: Point,
            bound: Binder {
                kinds: [],
                term: StructBoundData {
                    fields: [
                        FieldDecl {
                            name: x,
                            ty: Integer,
                        },
                        FieldDecl {
                            name: y,
                            ty: Integer,
                        },
                    ],
                },
            },
        }
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_struct_ty_empty_args() {
    let r: Ty = term("Point");
    expect_test::expect![[r#"
        StructTy(
            StructTy {
                id: Point,
                parameters: [],
            },
        )
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_struct_ty_no_args() {
    let r: Ty = term("Point");
    expect_test::expect![[r#"
        StructTy(
            StructTy {
                id: Point,
                parameters: [],
            },
        )
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_vec_int_ty() {
    let r: Ty = term("Vec<integer>");
    expect_test::expect![[r#"
        StructTy(
            StructTy {
                id: Vec,
                parameters: [
                    Ty(
                        Integer,
                    ),
                ],
            },
        )
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_expression() {
    let r: Expr = term("3 + 5 * 6");
    expect_test::expect![[r#"
        Add(
            IntegerLiteral(
                3,
            ),
            Mul(
                IntegerLiteral(
                    5,
                ),
                IntegerLiteral(
                    6,
                ),
            ),
        )
    "#]]
    .assert_debug_eq(&r);
}
