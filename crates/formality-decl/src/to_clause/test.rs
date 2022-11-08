#![cfg(test)]

use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::grammar::Program;

#[test]
fn to_clause() {
    let program: Program = term(
        "[
        crate foo {
            impl<ty X> Debug(X) where [] {}
        } 
        ]",
    );
    let clauses = program.to_clauses();

    expect![[r#"
        [
            for_all(<ty> implies([], has_impl(Debug(^ty0_0)))),
            for_all(<lt, ty> implies([outlives(^ty0_1, ^lt0_0)], well_formed_ty((rigid &(shared) ^lt0_0 ^ty0_1)))),
            for_all(<lt, ty> implies([outlives(^ty0_1, ^lt0_0)], well_formed_ty((rigid &(mut) ^lt0_0 ^ty0_1)))),
        ]
    "#]]
    .assert_debug_eq(&clauses);
}
