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
            for_all(<ty> implies([], has_impl(Debug(^0_0)))),
            for_all(<lt, ty> implies([outlives(^0_1, ^0_0)], well_formed_ty((rigid &(shared) ^0_0 ^0_1)))),
            for_all(<lt, ty> implies([outlives(^0_1, ^0_0)], well_formed_ty((rigid &(mut) ^0_0 ^0_1)))),
        ]
    "#]]
    .assert_debug_eq(&clauses);
}
