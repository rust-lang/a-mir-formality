#![cfg(test)]

use crate::rust::{term, try_term};
use formality_macros::test;

use crate::grammar::Program;

#[test]
fn test_parse_rust_like_trait_impl_syntax() {
    let r: Program = term(
        "[
            crate core {
                impl<A, B> PartialEq<A> for B {

                }
            }
        ]",
    );

    // Note: the for etc are correctly accounted.
    expect_test::expect![[r#"
        Program {
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
                                        self_ty: Ty {
                                            data: Variable(
                                                ^ty0_1,
                                            ),
                                        },
                                        trait_parameters: [
                                            Ty(
                                                Ty {
                                                    data: Variable(
                                                        ^ty0_0,
                                                    ),
                                                },
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
    let r: Program = term(
        "[
            crate core {
                trait Foo<A> where A : Bar<Self> {

                }
            }
        ]",
    );

    // Note: two type parameters, and the 0th one is self:
    expect_test::expect![[r#"
        Program {
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
    let r: Program = term(
        "[
            crate core {
                struct Foo<A> {
                    a : A,
                }
            }
        ]",
    );

    expect_test::expect![[r#"
        Program {
            crates: [
                Crate {
                    id: core,
                    items: [
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
                                                ty: Ty {
                                                    data: Variable(
                                                        ^ty0_0,
                                                    ),
                                                },
                                            },
                                        ],
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
fn test_parse_closure_def_directly() {
    let r: Result<crate::grammar::ClosureDef, _> =
        try_term("closure my_cl [] () -> u32 { trusted }");
    eprintln!("closure parse result: {:?}", r);
    assert!(r.is_ok(), "Failed to parse: {:?}", r.unwrap_err());
}

#[test]
fn test_parse_program_with_closure() {
    let r: Result<Program, _> = try_term("[crate Foo { closure my_cl [] () -> u32 { trusted } }]");
    eprintln!("program parse result: {:?}", r);
    assert!(r.is_ok(), "Failed to parse: {:?}", r.unwrap_err());
}
