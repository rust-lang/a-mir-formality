use expect_test::expect;
use formality_macros::test;
use formality_types::{
    grammar::{Ty, Wc, Wcs},
    parse::{term, term_with},
};

use crate::program::Program;

use super::with_bindings;

/// Simple example program consisting only of two trait declarations.
const PROGRAM: &str = "
    program(
        [trait Foo<ty Self> where {}],
        [impl<ty T> Foo(Vec<T>) where {}],
        [],
        []
    )
";

// Test that `exists<T> is_implemented(Foo(U))` yields `U = Vec<X>` for some fresh `X`
#[test]
fn exists_u_for_t() {
    let program: Program = term(PROGRAM);
    let assumptions: Wcs = Wcs::t();
    with_bindings(|u: Ty| {
        let goal: Wc = term_with(&[("U", &u)], "is_implemented(Foo(U))");
        let constraints = crate::prove::prove_wc_list(program, assumptions, goal);
        expect![[r#"
            (
                ?ty_0,
                {
                    <ty> Constraints { substitution: Substitution { map: {?ty_0: (rigid (adt Vec) ^ty0_0)} } },
                },
            )
        "#]]
        .assert_debug_eq(&(u, constraints));
    });
}
