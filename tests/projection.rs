use a_mir_formality::test_where_clause;
use formality_core::test_util::ResultTestExt;

const NORMALIZE_BASIC: &str = "[
    crate test {
        trait Iterator {
            type Item : [];
        }

        struct Vec<ty T> {}

        struct Foo {}

        impl<ty T> Iterator for Vec<T> {
            type Item = T;
        }
    }
]";

#[test]
fn normalize_basic() {
    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> {} => { <Vec<T> as Iterator>::Item = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => <Vec<!ty_1> as Iterator>::Item} }, Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> {} => { Iterator(Vec<T>), <Vec<T> as Iterator>::Item = T }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> { Iterator(T), <T as Iterator>::Item = Foo } => { <T as Iterator>::Item = Foo }",
    ).assert_ok(
        expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }}"]
    );

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> { Iterator(T) } => { <T as Iterator>::Item = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => <!ty_1 as Iterator>::Item} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> { Iterator(T) } => { <T as Iterator>::Item = <T as Iterator>::Item }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> { Iterator(T) } => { <T as Iterator>::Item = <U as Iterator>::Item }",
    ).assert_ok(
    expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }, Constraints { env: Env { variables: [!ty_1, ?ty_3, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => Vec<<!ty_1 as Iterator>::Item>, ?ty_3 => <!ty_1 as Iterator>::Item} }}"]);
}

const NORMALIZE_INTO_ITERATOR: &str = "[
    crate test {
        trait IntoIterator {
            type Item : [];
        }

        trait Iterator {
            type Item : [];
        }

        struct Vec<ty T> {}

        struct Foo {}

        impl<ty T> IntoIterator for Vec<T> {
            type Item = T;
        }

        impl<ty T> IntoIterator for T where  T: Iterator  {
            type Item = <T as Iterator>::Item;
        }
    }
]";

#[test]
fn normalize_into_iterator() {
    test_where_clause(
        NORMALIZE_INTO_ITERATOR,
        "forall<ty T> exists<ty U> {} => { <Vec<T> as IntoIterator>::Item = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => <Vec<!ty_1> as IntoIterator>::Item} }, Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);
}

const PROJECTION_EQUALITY: &str = "[
    crate test {
        trait Trait1<> {
            type Type : [];
        }
        trait Trait2<ty T> {}
        impl<ty T, ty U> Trait2<T> for U where  U: Trait1<>, <S as Trait1>::Type => T  {}
        struct S {}
        impl Trait1<> for S {
            type Type = u32;
        }
    }
]";

#[test]
fn projection_equality() {
    test_where_clause(
        PROJECTION_EQUALITY,
        "exists<ty U> {} => { Trait1(S), <S as Trait1<>>::Type = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => <S as Trait1>::Type} }}"]);

    test_where_clause(PROJECTION_EQUALITY, "exists<ty U> {} => { Trait2(S, U) }").assert_ok(
        expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => <S as Trait1>::Type} }}"],
    );
}

const TEST_TY_IS_INT: &str = "[
    crate test {
        trait Id {
            type This: [];
        }

        impl<ty T> Id for T {
            type This = T;
        }
    }
]";

#[test]
fn test_ty_is_int() {
    test_where_clause(
        TEST_TY_IS_INT,
        "{} => { <u16 as Id>::This = u16 }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }}"]);

    test_where_clause(
        TEST_TY_IS_INT,
        "{} => { @is_int(<u16 as Id>::This) }",
    )
    .assert_err(expect_test::expect![[r#"
        judgment `prove { goal: {@ is_int(<u16 as Id>::This)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait Id <ty> ], [impl <ty> Id(^ty0_0)], [], [alias <ty> <^ty0_0 as Id>::This = ^ty0_0], [], [], {Id}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {@ is_int(<u16 as Id>::This)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: @ is_int(<u16 as Id>::This), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "ty is int" failed at step #0 (src/file.rs:LL:CC) because
                    judgment had no applicable rules: `is_int { goal: <u16 as Id>::This, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`"#]]);
}
