use a_mir_formality::test_where_clause;

const CALLABLE: &str = "[
    crate test {
        trait FnLike<Args> { type Output: []; }
        trait Family { type View<'a>: []; }
    }
]";

#[test]
fn one_callable_and_one_complete_input_determine_one_output() {
    assert!(test_where_clause(
        CALLABLE,
        "forall<F, I, O, P> {
            FnLike(F, (I,)),
            <F as FnLike<(I,)>>::Output = O,
            <F as FnLike<(I,)>>::Output = P
        } => { O = P }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn equal_complete_inputs_preserve_output_determinacy() {
    assert!(test_where_clause(
        CALLABLE,
        "forall<F, I, J, O, P> {
            FnLike(F, (I,)), FnLike(F, (J,)), I = J,
            <F as FnLike<(I,)>>::Output = O,
            <F as FnLike<(J,)>>::Output = P
        } => { O = P }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn independent_complete_inputs_do_not_determine_equal_outputs() {
    assert!(!test_where_clause(
        CALLABLE,
        "forall<F, I, J, O, P> {
            FnLike(F, (I,)), FnLike(F, (J,)),
            <F as FnLike<(I,)>>::Output = O,
            <F as FnLike<(J,)>>::Output = P
        } => { O = P }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn independent_callables_do_not_determine_equal_outputs() {
    assert!(!test_where_clause(
        CALLABLE,
        "forall<F, G, I, O, P> {
            FnLike(F, (I,)), FnLike(G, (I,)),
            <F as FnLike<(I,)>>::Output = O,
            <G as FnLike<(I,)>>::Output = P
        } => { O = P }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn equal_outputs_do_not_recover_the_complete_inputs() {
    assert!(!test_where_clause(
        CALLABLE,
        "forall<F, I, J> {
            FnLike(F, (I,)), FnLike(F, (J,)),
            <F as FnLike<(I,)>>::Output = <F as FnLike<(J,)>>::Output
        } => { I = J }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn higher_ranked_input_equality_preserves_callable_outputs() {
    assert!(test_where_clause(
        CALLABLE,
        "forall<F, T, U> {
            Family(T), Family(U),
            for<'a> FnLike(F, (<T as Family>::View<'a>,)),
            for<'a> <T as Family>::View<'a> = <U as Family>::View<'a>
        } => {
            for<'b> <F as FnLike<(<T as Family>::View<'b>,)>>::Output =
                <F as FnLike<(<U as Family>::View<'b>,)>>::Output
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn higher_ranked_output_equality_instantiates_at_each_lifetime() {
    assert!(test_where_clause(
        CALLABLE,
        "forall<F, T, 'a, 'b> {
            Family(T),
            for<'c> FnLike(F, (<T as Family>::View<'c>,)),
            for<'c> <F as FnLike<(<T as Family>::View<'c>,)>>::Output =
                <T as Family>::View<'c>
        } => {
            <F as FnLike<(<T as Family>::View<'a>,)>>::Output = <T as Family>::View<'a>,
            <F as FnLike<(<T as Family>::View<'b>,)>>::Output = <T as Family>::View<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn one_lifetime_instance_does_not_establish_a_higher_ranked_output() {
    assert!(!test_where_clause(
        CALLABLE,
        "forall<F, T, 'a> {
            Family(T), FnLike(F, (<T as Family>::View<'a>,)),
            <F as FnLike<(<T as Family>::View<'a>,)>>::Output = <T as Family>::View<'a>
        } => {
            for<'b> <F as FnLike<(<T as Family>::View<'b>,)>>::Output =
                <T as Family>::View<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn equal_gat_inputs_determine_outputs_without_recovering_lifetimes() {
    let assumptions = "forall<F, T, 'a, 'b> {
        Family(T),
        FnLike(F, (<T as Family>::View<'a>,)),
        <T as Family>::View<'a> = <T as Family>::View<'b>
    }";
    assert!(test_where_clause(
        CALLABLE,
        &format!(
            "{assumptions} => {{
                <F as FnLike<(<T as Family>::View<'a>,)>>::Output =
                <F as FnLike<(<T as Family>::View<'b>,)>>::Output
            }}"
        ),
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    for goal in ["'a = 'b", "'a: 'b", "'b: 'a"] {
        assert!(
            !test_where_clause(CALLABLE, &format!("{assumptions} => {{ {goal} }}"))
                .iter()
                .any(|(c, _)| c.known_true)
        );
    }
}

#[test]
fn higher_ranked_output_can_be_used_for_its_input_lifetime() {
    assert!(test_where_clause(
        CALLABLE,
        "forall<F> {
            for<'a> FnLike(F, (&'a u32,)),
            for<'a> <F as FnLike<(&'a u32,)>>::Output = &'a u32
        } => {
            for<'b> <F as FnLike<(&'b u32,)>>::Output: 'b
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn output_can_be_used_for_a_shorter_lifetime() {
    assert!(test_where_clause(
        CALLABLE,
        "forall<F, 'a, 'b> {
            for<'c> FnLike(F, (&'c u32,)),
            for<'c> <F as FnLike<(&'c u32,)>>::Output = &'c u32,
            'a: 'b
        } => {
            <F as FnLike<(&'a u32,)>>::Output: 'b
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn outlives_respects_equality_of_either_operand() {
    for assertion in [
        "forall<T, U, 'a> { T = U, U: 'a } => { T: 'a }",
        "forall<'a, 'b, 'c> { 'a = 'b, 'b: 'c } => { 'a: 'c }",
        "forall<'a, 'b, 'c> { 'a: 'b, 'b = 'c } => { 'a: 'c }",
    ] {
        assert!(test_where_clause(CALLABLE, assertion)
            .iter()
            .any(|(c, _)| c.unconditionally_true()));
    }
}

#[test]
fn outlives_normalization_preserves_the_associated_item_condition() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            trait Family {
                type View<'a>: [Item::Out => &'a u32] where 'a: 'static;
            }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<T, 'a> { Family(T), 'a: 'static } => {
            <<T as Family>::View<'a> as Item>::Out: 'a
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<T, 'a> { Family(T) } => {
            <<T as Family>::View<'a> as Item>::Out: 'a
        }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn higher_ranked_reuse_does_not_extend_the_output_lifetime() {
    let assumptions = "forall<F> {
        for<'a> FnLike(F, (&'a u32,)),
        for<'a> <F as FnLike<(&'a u32,)>>::Output = &'a u32
    }";
    for goal in [
        "for<'b> <F as FnLike<(&'b u32,)>>::Output = &'static u32",
        "for<'b> <F as FnLike<(&'b u32,)>>::Output: 'static",
    ] {
        assert!(
            !test_where_clause(CALLABLE, &format!("{assumptions} => {{ {goal} }}"))
                .iter()
                .any(|(c, _)| c.known_true)
        );
    }
}

#[test]
fn output_determinacy_preserves_the_associated_item_condition() {
    let program = "[
        crate test {
            trait FnLike<Args> { type Output: []; }
            trait Item { type Out: []; }
            trait Family {
                type View<'a>: [Item::Out => &'a u32] where 'a: 'static;
            }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<F, T, O> {
            Family(T), FnLike(F, (&'static u32,)),
            <F as FnLike<(&'static u32,)>>::Output = O
        } => {
            <F as FnLike<(<<T as Family>::View<'static> as Item>::Out,)>>::Output = O
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<F, T, O, 'a> {
            Family(T), FnLike(F, (&'a u32,)),
            <F as FnLike<(&'a u32,)>>::Output = O
        } => {
            <F as FnLike<(<<T as Family>::View<'a> as Item>::Out,)>>::Output = O
        }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}
