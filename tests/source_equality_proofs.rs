use formality_rust::{check::check_all_crates, grammar::Crates, rust::try_term};

fn source_proves(test: &str) -> bool {
    let source = format!(
        "[crate proof {{
            trait Item {{ type Out: []; }}
            trait Family {{ type View<'a>: []; }}
            trait FnLike<Args> {{ type Output: []; }}
            trait Marker {{}}
            {test}
        }}]"
    );
    let program: Crates = try_term(&source).unwrap();
    check_all_crates(&program).check_proven().is_ok()
}

#[test]
fn source_equality_normalizes_callable_arguments() {
    assert!(source_proves(
        "test<T, U, F> where
            T: Item, U: Item,
            F: FnLike<(<T as Item>::Out,)>,
            <T as Item>::Out => <U as Item>::Out
        {
            F: FnLike<(<U as Item>::Out,)>
        }"
    ));
}

#[test]
fn source_equality_composes_across_projections() {
    assert!(source_proves(
        "test<T, U> where
            T: Item, U: Item,
            <T as Item>::Out => <U as Item>::Out,
            <U as Item>::Out => u32
        {
            <T as Item>::Out => u32
        }"
    ));
}

#[test]
fn source_input_equality_preserves_callable_outputs() {
    assert!(source_proves(
        "test<T, U, F, O> where
            T: Item, U: Item,
            F: FnLike<(<T as Item>::Out,)>,
            F: FnLike<(<U as Item>::Out,)>,
            <T as Item>::Out => <U as Item>::Out,
            <F as FnLike<(<T as Item>::Out,)>>::Output => O
        {
            <F as FnLike<(<U as Item>::Out,)>>::Output => O
        }"
    ));
}

#[test]
fn source_equality_preserves_outlives() {
    assert!(source_proves(
        "test<T, 'a, 'b> where
            T: Item, 'a: 'b,
            <T as Item>::Out => &'a u32
        {
            <T as Item>::Out: 'b
        }"
    ));
}

#[test]
fn nested_source_equality_preserves_outlives() {
    assert!(source_proves(
        "test<T, U, 'a> where
            T: Item, U: Item, <T as Item>::Out: Item,
            <T as Item>::Out => U,
            <U as Item>::Out => &'a u32
        {
            <<T as Item>::Out as Item>::Out: 'a
        }"
    ));
}

#[test]
fn higher_ranked_source_equality_normalizes_callable_arguments() {
    assert!(source_proves(
        "test<T, U, F> where
            T: Family, U: Family,
            for<'a> F: FnLike<(<T as Family>::View<'a>,)>,
            for<'a> <T as Family>::View<'a> => <U as Family>::View<'a>
        {
            for<'b> F: FnLike<(<U as Family>::View<'b>,)>
        }"
    ));
}

#[test]
fn higher_ranked_source_equality_preserves_outlives() {
    assert!(source_proves(
        "test<T> where
            T: Family,
            for<'a> <T as Family>::View<'a> => &'a u32
        {
            for<'b> <T as Family>::View<'b>: 'b
        }"
    ));
}

#[test]
fn source_callable_transport_requires_the_input_equality() {
    assert!(!source_proves(
        "test<T, U, F> where
            T: Item, U: Item,
            F: FnLike<(<T as Item>::Out,)>
        {
            F: FnLike<(<U as Item>::Out,)>
        }"
    ));
}

#[test]
fn source_equality_cannot_extend_an_output_lifetime() {
    assert!(!source_proves(
        "test<T> where
            T: Family,
            for<'a> <T as Family>::View<'a> => &'a u32
        {
            for<'b> <T as Family>::View<'b>: 'static
        }"
    ));
}

#[test]
fn source_equality_does_not_make_projections_injective() {
    assert!(!source_proves(
        "test<T, U> where
            T: Item, U: Item, U: Marker,
            <T as Item>::Out => <U as Item>::Out
        {
            T: Marker
        }"
    ));
}

#[test]
fn source_equality_does_not_recover_projection_lifetimes() {
    assert!(!source_proves(
        "test<T, 'a, 'b> where
            T: Family,
            <T as Family>::View<'a> => <T as Family>::View<'b>
        {
            'a: 'b
        }"
    ));
}

#[test]
fn growing_source_equality_remains_ambiguous_at_the_size_limit() {
    use formality_core::visit::CoreVisit;
    use formality_rust::{
        grammar::{Test, TestBoundData},
        prove::{prove, Bias, Env, ToWcs},
    };

    let program: Crates = try_term("[crate proof { trait Item<T> { type Out: []; } }]").unwrap();
    let _ = check_all_crates(&program).check_proven().unwrap();
    let test: Test = try_term(
        "test where
            for<T> (): Item<T>,
            for<T> <() as Item<T>>::Out => <() as Item<(T,)>>::Out
        {
            <() as Item<u32>>::Out => bool
        }",
    )
    .unwrap();
    let (env, TestBoundData { assumptions, goals }) =
        Env::new_with_bias(Bias::Completeness).instantiate_universally(&test.binder);
    let (assumptions, goals) = (assumptions.to_wcs(), goals.to_wcs());
    let mut decls = program.to_prove_decls();
    decls.max_size = (&assumptions, &goals).size() + 3;
    let result = prove(decls, env, assumptions, goals);
    assert!(result.iter().any(|(c, _)| !c.known_true));
    assert!(!result.iter().any(|(c, _)| c.known_true));
}
