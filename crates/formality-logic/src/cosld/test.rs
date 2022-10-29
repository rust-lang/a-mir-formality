#![cfg(test)]

use formality_infer::Env;
use formality_macros::test;
use formality_types::{db::mock::MockDatabase, parse::term};

#[test]
fn simple_test() {
    let db = MockDatabase::new()
        .with_program_clause(
            "for_all(<ty T> implies([is_implemented(Debug(T))], is_implemented(Debug(Vec<T>))))",
        )
        .with_program_clause("is_implemented(Debug(u32))")
        .into_db();
    let env = Env::default();

    let results = super::prove(&db, &env, &[], &term("is_implemented(Debug(Vec<u32>))"));

    expect_test::expect![[r#"
            {
                yes(
                    env(
                        U(0),
                        [
                            inference_var_data(
                                ty,
                                U(0),
                                Some(
                                    (rigid (scalar u32)),
                                ),
                                [],
                                [],
                                [],
                                [],
                            ),
                        ],
                        no,
                    ),
                ),
            }
        "#]]
    .assert_debug_eq(&results);
}
