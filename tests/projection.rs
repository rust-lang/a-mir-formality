use a_mir_formality::test_where_clause;

const NORMALIZE_BASIC: &str = "[
    crate test {
        trait Iterator<> where [] {
            type Item<> : [] where [];
        }

        struct Vec<ty T> where [] {}

        struct Foo<> where [] {}

        impl<ty T> Iterator<> for Vec<T> where [] {
            type Item<> = T where [];
        }
    }
]";

#[test]
fn normalize_basic() {
    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                            ?ty_2,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_2 => (Iterator::Item)<(rigid (adt Vec) !ty_1)>,
                    },
                },
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                            ?ty_2,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_2 => !ty_1,
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> {} => { <Vec<T> as Iterator>::Item<> = U }",
    ));

    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {},
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> {} => { Iterator(Vec<T>), <Vec<T> as Iterator<>>::Item<> = T }",
    ));

    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {},
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> { Iterator(T), <T as Iterator<>>::Item<> = Foo } => { <T as Iterator<>>::Item<> = Foo }",
    ));

    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                            ?ty_2,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_2 => (Iterator::Item)<!ty_1>,
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> { Iterator(T) } => { <T as Iterator<>>::Item<> = U }",
    ));

    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {},
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> { Iterator(T) } => { <T as Iterator<>>::Item<> = <T as Iterator<>>::Item<> }",
    ));

    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                            ?ty_2,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_2 => !ty_1,
                    },
                },
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                            ?ty_3,
                            ?ty_2,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_2 => (rigid (adt Vec) (Iterator::Item)<!ty_1>),
                        ?ty_3 => (Iterator::Item)<!ty_1>,
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        NORMALIZE_BASIC,
        "forall<ty T> exists<ty U> { Iterator(T) } => { <T as Iterator<>>::Item<> = <U as Iterator<>>::Item<> }",
    ));
}

const NORMALIZE_INTO_ITERATOR: &str = "[
    crate test {
        trait IntoIterator<> where [] {
            type Item<> : [] where [];
        }

        trait Iterator<> where [] {
            type Item<> : [] where [];
        }

        struct Vec<ty T> where [] {}

        struct Foo<> where [] {}

        impl<ty T> IntoIterator<> for Vec<T> where [] {
            type Item<> = T where [];
        }

        impl<ty T> IntoIterator<> for T where [ T: Iterator<> ] {
            type Item<> = <T as Iterator>::Item<> where [];
        }
    }
]";

#[test]
fn normalize_into_iterator() {
    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                            ?ty_2,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_2 => (IntoIterator::Item)<(rigid (adt Vec) !ty_1)>,
                    },
                },
                Constraints {
                    env: Env {
                        variables: [
                            !ty_1,
                            ?ty_2,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_2 => !ty_1,
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        NORMALIZE_INTO_ITERATOR,
        "forall<ty T> exists<ty U> {} => { <Vec<T> as IntoIterator>::Item<> = U }",
    ));
}

const PROJECTION_EQUALITY: &str = "[
    crate test {
        trait Trait1<> where [] {
            type Type<> : [] where [];
        }
        trait Trait2<ty T> where [] {}
        impl<ty T, ty U> Trait2<T> for U where [ U: Trait1<>, <S as Trait1>::Type => T ] {}
        struct S<> where [] {}
        impl<> Trait1<> for S<> where [] {
            type Type<> = u32 where [];
        }
    }
]";

#[test]
fn projection_equality() {
    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            ?ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_1 => (rigid (scalar u32)),
                    },
                },
                Constraints {
                    env: Env {
                        variables: [
                            ?ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_1 => (Trait1::Type)<(rigid (adt S))>,
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        PROJECTION_EQUALITY,
        "exists<ty U> {} => { Trait1(S), <S as Trait1<>>::Type<> = U }",
    ));

    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            ?ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_1 => (rigid (scalar u32)),
                    },
                },
                Constraints {
                    env: Env {
                        variables: [
                            ?ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_1 => (Trait1::Type)<(rigid (adt S))>,
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        PROJECTION_EQUALITY,
        "exists<ty U> {} => { Trait2(S, U) }",
    ));
}
