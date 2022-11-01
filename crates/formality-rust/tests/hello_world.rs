use formality_rust::check_program;
use formality_types::parse::term;

const PROGRAM: &str = "[
    crate Foo {
        trait Foo<ty T> where [T: Bar<Self>] {}

        trait Bar<ty T> where [T: Baz<>] {}
        
        trait Baz<> where [] {}
    }
]";

#[test]
fn test() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Foo)",
                source: Error {
                    context: "prove_where_clause_well_formed([is_implemented(Bar(!U(1)_1, !U(1)_0))] => is_implemented(Bar(!U(1)_1, !U(1)_0))",
                    source: "could not prove `well_formed_trait_ref(Bar(!U(1)_1, !U(1)_0))`",
                },
            },
        )
    "#]].assert_debug_eq(&check_program(&term(PROGRAM)));
}
