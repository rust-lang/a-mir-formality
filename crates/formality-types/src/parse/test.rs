#![cfg(test)]

use super::term;
use crate::grammar::{AtomicPredicate, Goal, Ty};
use expect_test::expect;
use test_log::test;

#[test]
fn parse_atomic_predicate() {
    let program: AtomicPredicate = term("is_implemented(Debug<u32>)");
    expect![[r#"
        IsImplemented(
            TraitRef {
                trait_id: TraitId {
                    data: "Debug",
                },
                parameters: [
                    Ty(
                        Ty {
                            data: RigidTy(
                                RigidTy {
                                    name: ScalarId(
                                        U32,
                                    ),
                                    parameters: [],
                                },
                            ),
                        },
                    ),
                ],
            },
        )
    "#]]
    .assert_debug_eq(&program);
}

#[test]
fn parse_forall_goal() {
    let program: Goal = term("forall<ty T> is_implemented(Debug<T>)");
    expect![[r#"
        Goal {
            data: ForAll(
                Binder {
                    kinds: [
                        Ty,
                    ],
                    term: Goal {
                        data: AtomicPredicate(
                            IsImplemented(
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
                },
            ),
        }
    "#]]
    .assert_debug_eq(&program);
}

#[test]
fn parse_nested_binders() {
    // T should have a debruijn index of 1, U should have a debruijn index of 0
    let program: Goal = term("forall<ty T> exists<ty U> is_implemented(Debug<T, U>)");
    expect![[r#"
        Goal {
            data: ForAll(
                Binder {
                    kinds: [
                        Ty,
                    ],
                    term: Goal {
                        data: Exists(
                            Binder {
                                kinds: [
                                    Ty,
                                ],
                                term: Goal {
                                    data: AtomicPredicate(
                                        IsImplemented(
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
                                                                                index: 1,
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
                            },
                        ),
                    },
                },
            ),
        }
    "#]]
    .assert_debug_eq(&program);
}

#[test]
fn parse_all() {
    // T should have a debruijn index of 1 the first time, 0 the second time
    let program: Goal = term(
        "forall<ty T> all(
            exists<ty U> is_implemented(PartialEq<T, U>), 
            has_impl(Debug<T>),
        )",
    );
    expect![[r#"
        Goal {
            data: ForAll(
                Binder {
                    kinds: [
                        Ty,
                    ],
                    term: Goal {
                        data: All(
                            [
                                Goal {
                                    data: Exists(
                                        Binder {
                                            kinds: [
                                                Ty,
                                            ],
                                            term: Goal {
                                                data: AtomicPredicate(
                                                    IsImplemented(
                                                        TraitRef {
                                                            trait_id: TraitId {
                                                                data: "PartialEq",
                                                            },
                                                            parameters: [
                                                                Ty(
                                                                    Ty {
                                                                        data: Variable(
                                                                            BoundVar(
                                                                                BoundVar {
                                                                                    debruijn: Some(
                                                                                        DebruijnIndex {
                                                                                            index: 1,
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
                                        },
                                    ),
                                },
                                Goal {
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
                            ],
                        ),
                    },
                },
            ),
        }
    "#]]
    .assert_debug_eq(&program);
}

#[test]
fn parse_scalar_id() {
    let ty1: Ty = term("u8");
    let ty2: Ty = term("(rigid (scalar u8))");
    assert_eq!(ty1, ty2);
}

#[test]
fn parse_adts_with_parameters() {
    let ty1: Ty = term("Vec<String>");
    let ty2: Ty = term("(rigid (adt Vec) (rigid (adt String)))");
    assert_eq!(ty1, ty2);
}

#[test]
fn parse_assoc_type() {
    let ty1: Ty = term("<String as Iterator>::Item");
    let ty2: Ty = term("(alias (Iterator::Item) String)");
    assert_eq!(ty1, ty2);
}

#[test]
fn parse_gat() {
    let ty1: Ty = term("<String as Iterator>::Item<u32>");
    let ty2: Ty = term("(alias (Iterator::Item) String (rigid (scalar u32)))");
    assert_eq!(ty1, ty2);
}
