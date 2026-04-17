#![cfg(test)]

use crate::rust::term;
use formality_macros::test;

use crate::grammar::Crates;
use crate::grammar::expr::PlaceExpr;

#[test]
fn test_parse_rust_like_trait_impl_syntax() {
    let r: Crates = term(
        "[
            crate core {
                impl<A, B> PartialEq<A> for B {

                }
            }
        ]",
    );

    // Note: the for etc are correctly accounted.
    expect_test::expect![[r#"
        Crates {
            crates: [
                Crate {
                    id: core,
                    items: [
                        TraitImpl(
                            TraitImpl {
                                safety: Safe,
                                binder: Binder {
                                    kinds: [
                                        Ty,
                                        Ty,
                                    ],
                                    term: TraitImplBoundData {
                                        trait_id: PartialEq,
                                        self_ty: Variable(
                                            ^ty0_1,
                                        ),
                                        trait_parameters: [
                                            Ty(
                                                Variable(
                                                    ^ty0_0,
                                                ),
                                            ),
                                        ],
                                        where_clauses: [],
                                        impl_items: [],
                                    },
                                },
                            },
                        ),
                    ],
                },
            ],
        }
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_parse_rust_like_trait_syntax() {
    let r: Crates = term(
        "[
            crate core {
                trait Foo<A> where A : Bar<Self> {

                }
            }
        ]",
    );

    // Note: two type parameters, and the 0th one is self:
    expect_test::expect![[r#"
        Crates {
            crates: [
                Crate {
                    id: core,
                    items: [
                        Trait(
                            Trait {
                                safety: Safe,
                                id: Foo,
                                binder: <ty, ty> where ^ty0_1 : Bar <^ty0_0> { },
                            },
                        ),
                    ],
                },
            ],
        }
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_parse_rust_like_struct_syntax() {
    let r: Crates = term(
        "[
            crate core {
                struct Foo<A> {
                    a : A,
                }
            }
        ]",
    );

    expect_test::expect![[r#"
        Crates {
            crates: [
                Crate {
                    id: core,
                    items: [
                        AdtItem(
                            Struct(
                                Struct {
                                    id: Foo,
                                    binder: Binder {
                                        kinds: [
                                            Ty,
                                        ],
                                        term: StructBoundData {
                                            where_clauses: [],
                                            fields: [
                                                Field {
                                                    name: Id(
                                                        a,
                                                    ),
                                                    ty: Variable(
                                                        ^ty0_0,
                                                    ),
                                                },
                                            ],
                                        },
                                    },
                                },
                            ),
                        ),
                    ],
                },
            ],
        }
    "#]]
    .assert_debug_eq(&r);
}

#[test]
fn test_parse_trusted_fn() {
    let r: Crates = term(
        "[
            crate core {
              fn run() -> () {trusted}
            }
        ]",
    );

    expect_test::expect![[r#"
        Crates {
            crates: [
                Crate {
                    id: core,
                    items: [
                        Fn(
                            Fn {
                                id: run,
                                safety: Safe,
                                binder: Binder {
                                    kinds: [],
                                    term: FnBoundData {
                                        input_args: [],
                                        output_ty: RigidTy(
                                            (),
                                        ),
                                        where_clauses: [],
                                        body: FnBody(
                                            TrustedFnBody,
                                        ),
                                    },
                                },
                            },
                        ),
                    ],
                },
            ],
        }
    "#]]
    .assert_debug_eq(&r);
}

// 1. THE FAILING TEST (Demonstrates the bug)
#[test]
fn test_place_expr_ambiguity_deref_vs_field() {
    // BUG: Under Config B (Deref=1, Field=2), this panics with an ambiguity error.
    // It should parse as Deref( Field(p, f) ) because field binds tighter.
    // Under Config A (Deref=10, Field=1), it parses but gives the wrong AST: Field(Deref(p, f)).
    let p: PlaceExpr = term("*p.f");
    expect_test::expect![[r#""#]].assert_debug_eq(&p);
}

// 2. THE BASELINE TEST (Proves parens bypass the bug)
#[test]
fn test_place_expr_parens_override() {
    // This works perfectly and proves that explicit parens resolve the ambiguity.
    // It correctly parses as Field( Deref(p), f ).
    let p: PlaceExpr = term("(*p).f");
    expect_test::expect![[r#"
        PlaceExpr {
            data: Field {
                prefix: PlaceExpr {
                    data: Parens(
                        PlaceExpr {
                            data: Deref {
                                prefix: PlaceExpr {
                                    data: Var(
                                        p,
                                    ),
                                },
                            },
                        },
                    ),
                },
                field_name: Id(
                    f,
                ),
            },
        }
    "#]].assert_debug_eq(&p);
}

// 3. THE INFIX TEST (Proves associativity works for fields alone)
#[test]
fn test_place_expr_field_left_associativity() {
    // This works perfectly, proving the parser handles chained infix operators.
    // It parses as Field( Field(p, f), g ).
    let p: PlaceExpr = term("p.f.g");
    expect_test::expect![[r#"
        PlaceExpr {
            data: Field {
                prefix: PlaceExpr {
                    data: Field {
                        prefix: PlaceExpr {
                            data: Var(
                                p,
                            ),
                        },
                        field_name: Id(
                            f,
                        ),
                    },
                },
                field_name: Id(
                    g,
                ),
            },
        }
    "#]].assert_debug_eq(&p);
}

// 4. THE CHAIN TEST (Proves the bug cascades)
#[test]
fn test_place_expr_deref_with_field_chain() {
    // This will also panic under Config B, as it inherits the *p.f ambiguity.
    // It should parse as Deref( Field( Field(p, f), g ) )
    let p: PlaceExpr = term("*p.f.g");
    expect_test::expect![[r#""#]].assert_debug_eq(&p);
}