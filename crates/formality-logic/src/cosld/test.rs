#![cfg(test)]

use formality_core::with_tracing_logs;
use formality_types::{
    env::Env,
    grammar::{AtomicPredicate, AtomicRelation, Hypothesis, Invariant, ProgramClause},
    parse::term,
};

use crate::Db;

#[derive(Debug)]
struct MockDb {
    invariants: Vec<Invariant>,
    clauses: Vec<ProgramClause>,
}

impl MockDb {
    fn new(clauses: &str, invariants: &str) -> Self {
        MockDb {
            invariants: term(invariants),
            clauses: term(clauses),
        }
    }
}

impl crate::db::Database for MockDb {
    fn elaborate_relation(&self, _r: &AtomicRelation) -> Vec<Hypothesis> {
        vec![]
    }

    fn invariants_for_predicate(&self, _predicate: &AtomicPredicate) -> Vec<Invariant> {
        self.invariants.clone()
    }

    fn program_clauses(&self, _: &AtomicPredicate) -> Vec<ProgramClause> {
        self.clauses.clone()
    }
}

#[test]
fn simple_test() {
    with_tracing_logs(|| {
        let db = Db::new(MockDb::new(
            "[\
            for_all(<ty T> implies([is_implemented(Debug(T))], is_implemented(Debug(Vec<T>)))),\
            is_implemented(Debug(u32)),\
            ]",
            "[]",
        ));
        let env = Env::default();

        let results = super::prove(&db, &env, &[], &term("is_implemented(Debug(Vec<u32>))"));

        expect_test::expect![[r#"
            {
                Yes(
                    Env {
                        universe: Universe {
                            index: 0,
                        },
                        inference_data: [
                            InferenceVarData {
                                kind: Ty,
                                universe: Universe {
                                    index: 0,
                                },
                                mapped_to: Some(
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
                                ),
                                subtype_of: [],
                                supertype_of: [],
                                outlives: [],
                                outlived_by: [],
                            },
                        ],
                        coherence_mode: No,
                    },
                ),
            }
        "#]].assert_debug_eq(&results);
    })
}
