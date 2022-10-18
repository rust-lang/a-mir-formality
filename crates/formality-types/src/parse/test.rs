#![cfg(test)]

use super::term;
use crate::grammar::AtomicPredicate;
use expect_test::expect;

#[test]
fn to_clause_test() {
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
    "#]].assert_debug_eq(&program);
}
