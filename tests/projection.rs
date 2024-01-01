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
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], coherence_mode: false }, known_true: true, substitution: {?ty_2 => <Vec<!ty_1> as Iterator>::Item} }, Constraints { env: Env { variables: [!ty_1, ?ty_2], coherence_mode: false }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> {} => { Iterator(Vec<T>), <Vec<T> as Iterator>::Item = T }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], coherence_mode: false }, known_true: true, substitution: {} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> { Iterator(T), <T as Iterator>::Item = Foo } => { <T as Iterator>::Item = Foo }",
    ).assert_ok(
        expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], coherence_mode: false }, known_true: true, substitution: {} }}"]
    );

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> { Iterator(T) } => { <T as Iterator>::Item = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], coherence_mode: false }, known_true: true, substitution: {?ty_2 => <!ty_1 as Iterator>::Item} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> { Iterator(T) } => { <T as Iterator>::Item = <T as Iterator>::Item }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], coherence_mode: false }, known_true: true, substitution: {} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> { Iterator(T) } => { <T as Iterator>::Item = <U as Iterator>::Item }",
    ).assert_ok(
    expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], coherence_mode: false }, known_true: true, substitution: {?ty_2 => !ty_1} }, Constraints { env: Env { variables: [!ty_1, ?ty_3, ?ty_2], coherence_mode: false }, known_true: true, substitution: {?ty_2 => Vec<<!ty_1 as Iterator>::Item>, ?ty_3 => <!ty_1 as Iterator>::Item} }}"]);
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
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], coherence_mode: false }, known_true: true, substitution: {?ty_2 => <Vec<!ty_1> as IntoIterator>::Item} }, Constraints { env: Env { variables: [!ty_1, ?ty_2], coherence_mode: false }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);
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
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], coherence_mode: false }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], coherence_mode: false }, known_true: true, substitution: {?ty_1 => <S as Trait1>::Type} }}"]);

    test_where_clause(PROJECTION_EQUALITY, "exists<ty U> {} => { Trait2(S, U) }").assert_ok(
        expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], coherence_mode: false }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], coherence_mode: false }, known_true: true, substitution: {?ty_1 => <S as Trait1>::Type} }}"],
    );
}
