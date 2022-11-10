#![cfg(test)]

use formality_logic::{Db, Env};
use formality_types::parse::term;

use crate::grammar::Program;

#[test]
fn test1() {
    let program: Program = term(
        "[
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
        ]",
    );
    let db = Db::new(program);
    let env = Env::default();
    let result = formality_logic::prove_universal_goal(
        &db,
        &env,
        &[],
        &term("is_implemented(Debug(Vec<u32>))"),
    );
    expect_test::expect![[r#"
        yes
    "#]]
    .assert_debug_eq(&result);
}
