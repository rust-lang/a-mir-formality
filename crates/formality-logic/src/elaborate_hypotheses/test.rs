#![cfg(test)]

use expect_test;
use formality_types::{
    grammar::{AtomicPredicate, AtomicRelation, Hypothesis, Invariant},
    parse::term,
};
use test_log::test;

use super::elaborate_hypotheses;

struct MockDb {
    invariants: Vec<Invariant>,
}

impl crate::Db for MockDb {
    fn elaborate_relation(&self, _r: &AtomicRelation) -> Vec<Hypothesis> {
        vec![]
    }

    fn invariants_for_predicate(&self, _predicate: &AtomicPredicate) -> Vec<Invariant> {
        self.invariants.clone()
    }
}

#[test]
fn test_single_step() {
    let invariants: Vec<Invariant> = term(
        "
        [
            <ty X> is_implemented(Ord(X)) => is_implemented(PartialOrd(X))
        ]
        ",
    );

    let hypotheses: Vec<Hypothesis> = term(
        "
        [
            is_implemented(Ord(u32))
        ]
        ",
    );

    let hypotheses1 = elaborate_hypotheses(&MockDb { invariants }, &hypotheses);

    expect_test::expect![[r#"
        [
            Hypothesis {
                data: AtomicPredicate(
                    IsImplemented(
                        TraitRef {
                            trait_id: TraitId {
                                data: "Ord",
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
                    ),
                ),
            },
            Hypothesis {
                data: AtomicPredicate(
                    IsImplemented(
                        TraitRef {
                            trait_id: TraitId {
                                data: "PartialOrd",
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
                    ),
                ),
            },
        ]
    "#]]
    .assert_debug_eq(&hypotheses1);
}

#[test]
fn test_transitive() {
    let invariants: Vec<Invariant> = term(
        "
        [
            <ty X> is_implemented(A(X)) => is_implemented(B(X)),
            <ty X> is_implemented(B(X)) => is_implemented(C(X)),
        ]
        ",
    );

    let hypotheses: Vec<Hypothesis> = term(
        "
        [
            is_implemented(A(u32))
        ]
        ",
    );

    let hypotheses1 = elaborate_hypotheses(&MockDb { invariants }, &hypotheses);

    expect_test::expect![[r#"
        [
            Hypothesis {
                data: AtomicPredicate(
                    IsImplemented(
                        TraitRef {
                            trait_id: TraitId {
                                data: "A",
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
                    ),
                ),
            },
            Hypothesis {
                data: AtomicPredicate(
                    IsImplemented(
                        TraitRef {
                            trait_id: TraitId {
                                data: "B",
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
                    ),
                ),
            },
            Hypothesis {
                data: AtomicPredicate(
                    IsImplemented(
                        TraitRef {
                            trait_id: TraitId {
                                data: "C",
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
                    ),
                ),
            },
        ]
    "#]]
    .assert_debug_eq(&hypotheses1);
}
