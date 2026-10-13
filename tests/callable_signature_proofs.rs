use a_mir_formality::test_where_clause;

// Type-level cases from https://github.com/rust-lang/rust/pull/162745.
// Candidate output equations are conclusions, not premises of dependency checking.
const DECLARATIONS: &str = "[
    crate test {
        trait Family { type View<'a>: []; }
        trait Identity { type Output: [Family]; }
        trait Carrier { type Assoc: [Identity::Output => <Self as Carrier>::Assoc]; }
        trait Middle { type Next: [Identity::Output => <Self as Middle>::Next]; }
        trait Nested { type Assoc: [Middle::Next => <Self as Nested>::Assoc]; }
        trait Has<'r> { type Assoc: ['r]; }
        trait Get<'r> { type Output: [Family]; }
        impl<'r, T> Get<'r> for T where T: Family, T: 'r { type Output = T; }
    }
]";

#[test]
fn a_reference_input_determines_the_lifetime_of_a_different_output_type() {
    assert!(test_where_clause(
        DECLARATIONS,
        "forall<T, U, 'a, 'b> {
            T: 'a, T: 'b, U: 'a, U: 'b, &'a T = &'b T
        } => { @wf(&'a U), @wf(&'b U), &'a U = &'b U }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn carrier_equality_determines_the_candidate_output() {
    assert!(test_where_clause(
        DECLARATIONS,
        "forall<C, T, 'a, 'b> {
            Carrier(C), Family(T), <C as Carrier>::Assoc = T,
            <T as Family>::View<'a> = <T as Family>::View<'b>
        } => {
            Identity(T), <T as Identity>::Output = T,
            <<T as Identity>::Output as Family>::View<'a> =
            <<T as Identity>::Output as Family>::View<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn nested_declarations_determine_the_candidate_output() {
    assert!(test_where_clause(
        DECLARATIONS,
        "forall<C, T, 'a, 'b> {
            Nested(C), Family(T), <C as Nested>::Assoc = T,
            <T as Family>::View<'a> = <T as Family>::View<'b>
        } => {
            Identity(T), <T as Identity>::Output = T,
            <<T as Identity>::Output as Family>::View<'a> =
            <<T as Identity>::Output as Family>::View<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn declared_outlives_enables_the_impl_used_by_the_output() {
    assert!(test_where_clause(
        DECLARATIONS,
        "forall<C, T, 'r, 'a, 'b> {
            Has(C, 'r), Family(T), <C as Has<'r>>::Assoc = T,
            <T as Family>::View<'a> = <T as Family>::View<'b>
        } => {
            T: 'r, Get(T, 'r), <T as Get<'r>>::Output = T,
            <<T as Get<'r>>::Output as Family>::View<'a> =
            <<T as Get<'r>>::Output as Family>::View<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn missing_outlives_cannot_enable_the_output_impl() {
    assert!(!test_where_clause(
        DECLARATIONS,
        "forall<T, 'r> { Family(T) } => { <T as Get<'r>>::Output = T }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn declared_reference_bound_justifies_shortening() {
    let result = test_where_clause(
        DECLARATIONS,
        "forall<C, 'a, 'r> {
            Has(C, 'r), <C as Has<'r>>::Assoc = &'a ()
        } => { 'a: 'r, &'a () <: &'r () }",
    );
    assert!(result.iter().any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn quantified_declaration_can_supply_a_static_bound() {
    assert!(test_where_clause(
        DECLARATIONS,
        "forall<C, T> {
            Family(T), for<'r> Has(C, 'r),
            for<'r> <C as Has<'r>>::Assoc = T
        } => { T: 'static, <T as Get<'static>>::Output = T }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn either_declaration_path_can_supply_the_required_bound() {
    for relation in ["'a: 'r", "'b: 'r"] {
        let assertion = format!(
            "forall<C, D, T, 'a, 'b, 'r> {{
            Has(C, 'a), Has(D, 'b),
            <C as Has<'a>>::Assoc = T, <D as Has<'b>>::Assoc = T,
            {relation}
        }} => {{ T: 'r }}"
        );
        assert!(
            test_where_clause(DECLARATIONS, &assertion)
                .iter()
                .any(|(c, _)| c.unconditionally_true()),
            "{relation}"
        );
    }
    assert!(!test_where_clause(
        DECLARATIONS,
        "forall<C, D, T, 'a, 'b, 'r> {
            Has(C, 'a), Has(D, 'b),
            <C as Has<'a>>::Assoc = T, <D as Has<'b>>::Assoc = T
        } => { T: 'r }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn declared_gat_bound_can_hold_at_every_function_item_instance() {
    let program = "[crate test {
        trait Clone {}
        trait Family { type View<'a>: [Clone]; }
    }]";
    assert!(test_where_clause(
        program,
        "forall<T> { Family(T) } => { for<'a> Clone(<T as Family>::View<'a>) }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn a_static_only_function_item_cannot_be_generalized() {
    assert!(!test_where_clause(
        DECLARATIONS,
        "forall<T> { Family(T) } => { for<'a> 'a: 'static }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn one_instance_of_a_function_requirement_is_not_enough() {
    let program = "[crate test {
        trait Clone {}
        trait Family { type View<'a>: []; }
    }]";
    assert!(!test_where_clause(
        program,
        "forall<T> { Family(T), Clone(<T as Family>::View<'static>) } => {
            for<'a> Clone(<T as Family>::View<'a>)
        }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn a_reference_input_supplies_its_well_formedness_requirement() {
    assert!(test_where_clause(
        DECLARATIONS,
        "forall<T> { Family(T) } => {
            for<'a> if { @wf(&'a <T as Family>::View<'a>) }
                <T as Family>::View<'a>: 'a
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        DECLARATIONS,
        "forall<T> { Family(T) } => { for<'a> <T as Family>::View<'a>: 'a }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn declared_shortening_is_available_in_a_function_body() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Has<'r> { type Assoc: ['r]; }
            fn shorten<'a, 'r, C>(value: &'a ()) -> &'r ()
                where C: Has<'r>, <C as Has<'r>>::Assoc => &'a () {
                return value;
            }
        }
    ]"
    )
    .is_ok());
}

#[test]
fn a_conditional_gat_does_not_supply_its_own_static_premise() {
    let program = "[crate test {
        trait Conditional { type Assoc<'a>: ['a] where Self: 'a; }
    }]";
    assert!(!test_where_clause(
        program,
        "forall<C, T> {
            Conditional(C), for<'a> if { C: 'a } <C as Conditional>::Assoc<'a> = T
        } => { T: 'static }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
    assert!(test_where_clause(
        program,
        "forall<C, T> {
            Conditional(C), C: 'static,
            for<'a> if { C: 'a } <C as Conditional>::Assoc<'a> = T
        } => { T: 'static }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn a_scoped_type_argument_retains_its_reference_requirement() {
    let program = "[crate test {
        trait Bound<'a, U> where Self: 'a {}
        trait ScopedCarrier<U> {
            type Assoc<'a>: [Bound<'a, &'a U>] where U: 'a;
        }
    }]";
    for (premise, expected) in [("", false), (", U: 'static", true)] {
        let assertion = format!(
            "forall<C, T, U> {{
            ScopedCarrier(C, U), for<'a> if {{ U: 'a }} <C as ScopedCarrier<U>>::Assoc<'a> = T
            {premise}
        }} => {{ T: 'static }}"
        );
        let result = test_where_clause(program, &assertion);
        if expected {
            assert!(result.iter().any(|(c, _)| c.unconditionally_true()));
        } else {
            assert!(!result.iter().any(|(c, _)| c.known_true));
        }
    }
}

const NESTED_BINDERS: &str = "[crate test {
    trait Family { type View<'a, 'b>: []; }
    trait TripleFamily { type View<'a, 'b, 'c>: []; }
    trait Accepts<Arg> {}
}]";

#[test]
fn an_inner_callable_binder_can_be_renamed() {
    assert!(test_where_clause(
        NESTED_BINDERS,
        "forall<T, F, 'a> {
            Family(T), for<'b> Accepts(F, <T as Family>::View<'a, 'b>)
        } => { for<'c> Accepts(F, <T as Family>::View<'a, 'c>) }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn independent_inner_binders_can_be_reordered() {
    assert!(test_where_clause(
        NESTED_BINDERS,
        "forall<T, F, 'a> {
            TripleFamily(T), for<'b, 'c> Accepts(F, <T as TripleFamily>::View<'a, 'c, 'b>)
        } => { for<'d, 'e> Accepts(F, <T as TripleFamily>::View<'a, 'd, 'e>) }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn an_inner_binder_does_not_change_an_outer_lifetime() {
    for goal in [
        "for<'c> Accepts(F, <T as Family>::View<'other, 'c>)",
        "for<'c> Accepts(F, <T as Family>::View<'c, 'a>)",
    ] {
        assert!(!test_where_clause(
            NESTED_BINDERS,
            &format!(
                "forall<T, F, 'a, 'other> {{
                Family(T), for<'b> Accepts(F, <T as Family>::View<'a, 'b>)
            }} => {{ {goal} }}"
            ),
        )
        .iter()
        .any(|(c, _)| c.known_true));
    }
}

#[test]
fn a_repeated_inner_lifetime_cannot_be_split_into_two_independent_ones() {
    assert!(!test_where_clause(
        NESTED_BINDERS,
        "forall<T, F, 'a> {
            TripleFamily(T), for<'b> Accepts(F, <T as TripleFamily>::View<'a, 'b, 'b>)
        } => { for<'c, 'd> Accepts(F, <T as TripleFamily>::View<'a, 'c, 'd>) }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn declared_identity_is_available_in_a_function_body() {
    let result = a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Family { type View<'a>: []; }
            trait Identity { type Output: [Family]; }
            trait Carrier { type Assoc: [Identity::Output => <Self as Carrier>::Assoc]; }
            fn identity<'a, C, T>(value: <T as Family>::View<'a>)
                -> <<T as Identity>::Output as Family>::View<'a>
                where C: Carrier, T: Family, <C as Carrier>::Assoc => T {
                return value;
            }
        }
    ]",
    );
    assert!(
        result.is_ok(),
        "{:?}",
        result
            .err()
            .map(|error| formality_core::test_util::format_error_leaves(&error))
    );
}

#[test]
fn declared_dependency_is_available_across_crates() {
    let result = a_mir_formality::test_program_ok(
        "[
        crate interface {
            trait Family { type View<'a>: []; }
            trait Identity { type Output: [Family]; }
            trait Carrier { type Assoc: [Identity::Output => <Self as Carrier>::Assoc]; }
        },
        crate consumer {
            test<C, T, 'a, 'b> where
                C: Carrier, T: Family, <C as Carrier>::Assoc => T,
                <T as Family>::View<'a> => <T as Family>::View<'b> {
                <<T as Identity>::Output as Family>::View<'a> =>
                    <<T as Identity>::Output as Family>::View<'b>
            }
        }
    ]",
    );
    assert!(
        result.is_ok(),
        "{:?}",
        result
            .err()
            .map(|error| formality_core::test_util::format_error_leaves(&error))
    );
}

#[test]
fn an_unrelated_lifetime_cannot_be_shortened_by_the_declaration() {
    assert!(!test_where_clause(
        DECLARATIONS,
        "forall<C, 'a, 'r, 'unrelated> {
            Has(C, 'r), <C as Has<'r>>::Assoc = &'a ()
        } => { 'unrelated: 'r }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn a_concrete_impl_and_a_declaration_give_the_same_type() {
    let program = "[crate test {
        struct Storage<T> { value: T }
        struct Wrapper<T> { value: T }
        trait Outer { type Inner: []; }
        impl<T> Outer for Wrapper<T> { type Inner = Storage<T>; }
        trait Carrier<T> { type Assoc: [Outer::Inner => Storage<T>]; }
    }]";
    for premises in ["", "Carrier(C, T), <C as Carrier<T>>::Assoc = Wrapper<T>"] {
        assert!(test_where_clause(
            program,
            &format!(
                "forall<C, T> {{ {premises} }} => {{
                <Wrapper<T> as Outer>::Inner = Storage<T>
            }}"
            ),
        )
        .iter()
        .any(|(c, _)| c.unconditionally_true()));
    }
}
