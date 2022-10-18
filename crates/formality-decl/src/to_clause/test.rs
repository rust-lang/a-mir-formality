#![cfg(test)]

use expect_test::expect;
use formality_types::parse::term;
use test_log::test;

use crate::grammar::Program;

#[test]
fn to_clause() {
    let program: Program = term(
        "
        crate foo {
            impl<ty X> Debug<X> where [] {}
        } 
        ",
    );
    let clauses = program.to_clauses();

    expect![[r#"
        [
            Hypothesis {
                data: ForAll(
                    Binder {
                        kinds: [
                            Ty,
                        ],
                        term: Hypothesis {
                            data: Implies(
                                [],
                                Hypothesis {
                                    data: AtomicPredicate(
                                        HasImpl(
                                            TraitRef {
                                                trait_id: TraitId {
                                                    data: "Debug",
                                                },
                                                parameters: [
                                                    Ty(
                                                        Ty {
                                                            data: Variable(
                                                                BoundVar(
                                                                    BoundVar {
                                                                        debruijn: Some(
                                                                            DebruijnIndex {
                                                                                index: 0,
                                                                            },
                                                                        ),
                                                                        var_index: VarIndex {
                                                                            index: 0,
                                                                        },
                                                                    },
                                                                ),
                                                            ),
                                                        },
                                                    ),
                                                ],
                                            },
                                        ),
                                    ),
                                },
                            ),
                        },
                    },
                ),
            },
        ]
    "#]].assert_debug_eq(&clauses);
}
