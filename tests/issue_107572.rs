use a_mir_formality::test_where_clause;

// https://github.com/rust-lang/rust/issues/107572
// The proposed bound is for<'a> FnOnce(A::Assoc<'a>) -> A::Assoc<'a>.
// Check its output dependency before assuming any callable Output equation:
// two valid lifetime instances with equal inputs must have equal outputs.
const GAT: &str = "[
    crate test {
        trait GAT { type Assoc<'a>: []; }
        enum Option<T> { None {}, Some { value: T } }

        struct Erased {}
        impl GAT for Erased { type Assoc<'a> = (); }

        struct Borrowed {}
        impl GAT for Borrowed { type Assoc<'a> = &'a u32; }
    }
]";

#[test]
fn the_original_output_depends_on_the_complete_input() {
    assert!(test_where_clause(
        GAT,
        "forall<A, 'a, 'b> {
            GAT(A), <A as GAT>::Assoc<'a> = <A as GAT>::Assoc<'b>
        } => {
            @wf(<A as GAT>::Assoc<'a>), @wf(<A as GAT>::Assoc<'b>),
            <A as GAT>::Assoc<'a> = <A as GAT>::Assoc<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn wrapping_the_complete_input_keeps_the_output_determined() {
    assert!(test_where_clause(
        GAT,
        "forall<A, 'a, 'b> {
            GAT(A), <A as GAT>::Assoc<'a> = <A as GAT>::Assoc<'b>
        } => {
            @wf(Option<<A as GAT>::Assoc<'a>>),
            @wf(Option<<A as GAT>::Assoc<'b>>),
            Option<<A as GAT>::Assoc<'a>> = Option<<A as GAT>::Assoc<'b>>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn independent_family_equalities_can_determine_the_output() {
    assert!(test_where_clause(
        GAT,
        "forall<A, B, 'a, 'b> {
            GAT(A), GAT(B),
            for<'x> <A as GAT>::Assoc<'x> = <B as GAT>::Assoc<'x>,
            <A as GAT>::Assoc<'a> = <A as GAT>::Assoc<'b>
        } => {
            @wf(<B as GAT>::Assoc<'a>), @wf(<B as GAT>::Assoc<'b>),
            <B as GAT>::Assoc<'a> = <B as GAT>::Assoc<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn unrelated_families_do_not_determine_the_output() {
    let premises = "forall<'a, 'b> {
        GAT(Erased), GAT(Borrowed),
        <Erased as GAT>::Assoc<'a> = <Erased as GAT>::Assoc<'b>
    }";
    assert!(test_where_clause(
        GAT,
        &format!(
            "{premises} => {{
            @wf(<Erased as GAT>::Assoc<'a>), @wf(<Erased as GAT>::Assoc<'b>),
            @wf(<Borrowed as GAT>::Assoc<'a>), @wf(<Borrowed as GAT>::Assoc<'b>)
        }}"
        ),
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        GAT,
        &format!(
            "{premises} => {{
            <Borrowed as GAT>::Assoc<'a> = <Borrowed as GAT>::Assoc<'b>
        }}"
        ),
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn an_extra_output_reference_is_not_determined_by_an_erased_input() {
    let premises = "forall<'a, 'b> {
        GAT(Erased),
        <Erased as GAT>::Assoc<'a> = <Erased as GAT>::Assoc<'b>
    }";
    assert!(test_where_clause(
        GAT,
        &format!(
            "{premises} => {{
            @wf((<Erased as GAT>::Assoc<'a>, &'a ())),
            @wf((<Erased as GAT>::Assoc<'b>, &'b ()))
        }}"
        ),
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        GAT,
        &format!(
            "{premises} => {{
            (<Erased as GAT>::Assoc<'a>, &'a ()) =
            (<Erased as GAT>::Assoc<'b>, &'b ())
        }}"
        ),
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn a_gat_requirement_must_hold_for_both_lifetime_instances() {
    let program = "[
        crate test {
            trait Lending { type Assoc<'a>: [] where Self: 'a; }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<A, 'a, 'b> {
            Lending(A), A: 'a, A: 'b,
            <A as Lending>::Assoc<'a> = <A as Lending>::Assoc<'b>
        } => {
            @wf(<A as Lending>::Assoc<'a>), @wf(<A as Lending>::Assoc<'b>),
            <A as Lending>::Assoc<'a> = <A as Lending>::Assoc<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<A, 'a, 'b> { Lending(A), A: 'a } => {
            @wf(<A as Lending>::Assoc<'b>)
        }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}
