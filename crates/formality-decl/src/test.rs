#![cfg(test)]

use formality_infer::Env;
use formality_types::{db::Db, parse::term};

use crate::grammar::Program;

#[test]
fn test1() {
    let program: Program = term(
        "
        crate core {
            trait Debug<ty Self> where [] {}

            struct Vec<ty T> where [] {
                struct { }
            }
            
            struct Vec<ty T> where [] {
                struct { }
            }
            
            impl<ty T> Debug(Vec<T>) where [] {}
            impl<> Debug(u32) where [] {}
        }
    ",
    );
    let db = Db::new(program);
    let env = Env::default();
    let result =
        formality_logic::cosld::prove(&db, &env, &[], &term("is_implemented(Debug(Vec<u32>))"));
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
                                (rigid (adt Vec) (rigid (scalar u32))),
                            ),
                            [],
                            [],
                            [],
                            [],
                        ),
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
                        inference_var_data(
                            ty,
                            U(0),
                            Some(
                                (rigid (adt Vec) (rigid (scalar u32))),
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
    "#]].assert_debug_eq(&result);
}
