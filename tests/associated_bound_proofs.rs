use a_mir_formality::test_where_clause;

const GUARDED_FAMILY: &str = "[
    crate test {
        trait Required {}
        trait Item { type Out: []; }
        trait Family<'r, T> {
            type View<'a>: [Item::Out => &'a T]
                where T: Required, T: 'a, 'r: 'a;
        }
    }
]";

#[test]
fn declared_equality_follows_for_every_instance_satisfying_its_premises() {
    assert!(test_where_clause(
        GUARDED_FAMILY,
        "forall<C, T, 'r> { Family(C, 'r, T) } => {
            for<'a> if { Required(T), T: 'a, 'r: 'a }
                <<C as Family<'r, T>>::View<'a> as Item>::Out = &'a T
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn none_of_the_declared_equality_premises_can_be_dropped() {
    for premises in ["T: 'a, 'r: 'a", "Required(T), 'r: 'a", "Required(T), T: 'a"] {
        let assertion = format!(
            "forall<C, T, 'r, 'a> {{ Family(C, 'r, T), {premises} }} => {{
                <<C as Family<'r, T>>::View<'a> as Item>::Out = &'a T
            }}"
        );
        assert!(
            !test_where_clause(GUARDED_FAMILY, &assertion)
                .iter()
                .any(|(c, _)| c.known_true),
            "{premises}"
        );
    }
}

#[test]
fn the_same_premises_guard_associated_trait_membership() {
    assert!(test_where_clause(
        GUARDED_FAMILY,
        "forall<C, T, 'r> { Family(C, 'r, T) } => {
            for<'a> if { Required(T), T: 'a, 'r: 'a }
                Item(<C as Family<'r, T>>::View<'a>)
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        GUARDED_FAMILY,
        "forall<C, T, 'r, 'a> { Family(C, 'r, T), T: 'a, 'r: 'a } => {
            Item(<C as Family<'r, T>>::View<'a>)
        }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn declaration_premises_can_follow_from_transitive_outlives() {
    assert!(test_where_clause(
        GUARDED_FAMILY,
        "forall<C, T, 'r, 'a, 'b> {
            Family(C, 'r, T), Required(T), T: 'b, 'r: 'b, 'b: 'a
        } => { <<C as Family<'r, T>>::View<'a> as Item>::Out = &'a T }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn distinct_instantiations_do_not_share_their_premises() {
    assert!(!test_where_clause(
        GUARDED_FAMILY,
        "forall<C, T, 'r, 'a, 'b> {
            Family(C, 'r, T), Required(T), T: 'a, 'r: 'a
        } => { <<C as Family<'r, T>>::View<'b> as Item>::Out = &'b T }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}
