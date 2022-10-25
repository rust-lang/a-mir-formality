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
            impl<ty X> Debug(X) where [] {}
        } 
        ",
    );
    let clauses = program.to_clauses();

    expect![[r#"
        [
            for_all(<ty> implies([], has_impl(Debug(^0_0)))),
        ]
    "#]]
    .assert_debug_eq(&clauses);
}
