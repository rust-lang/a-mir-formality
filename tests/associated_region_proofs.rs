use formality_core::Upcast;
use formality_rust::grammar::{
    Binder, Crates, Lt, ParameterKind, Predicate, UniversalVar, Wc, Wcs,
};
use formality_rust::prove::test_util::TestAssertionPart;
use formality_rust::prove::{prove, prove_normalize, Constrained, Constraints, Env, Program};
use formality_rust::rust::term;

#[test]
fn existential_obligations_use_one_witness_for_the_entire_conjunction() {
    use formality_rust::prove::test_util::test_prove;

    assert!(test_prove(
        Program::empty(),
        term("{} => { for<'a> exists<'x> { 'x = 'a, 'x: 'a } }"),
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_prove(
        Program::empty(),
        term("forall<'a> {} => { exists<'x> { 'x = 'static, 'x = 'a } }"),
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn a_witness_may_depend_only_on_universals_in_its_scope() {
    use formality_rust::prove::test_util::test_prove;

    assert!(test_prove(
        Program::empty(),
        term("{} => { for<'a> exists<T> { T = &'a u32, @wf(T) } }"),
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_prove(
        Program::empty(),
        term("{} => { exists<T> { for<'a> T = &'a u32 } }"),
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

const GUARDED_FAMILY: &str = "[
    crate test {
        trait Item { type Out: []; }
        trait Family {
            type View<'a>: [Item::Out => &'a u32] where 'a: 'static;
        }
    }
]";

fn declarations() -> Program {
    let crates: Crates = term(GUARDED_FAMILY);
    crates.to_prove_decls()
}

fn query(assertion: &str) -> (Env, Wcs, Wcs, Vec<UniversalVar>) {
    let TestAssertionPart::ForAll(binder) = term(assertion) else {
        panic!();
    };
    let (env, variables) = Env::default().universal_substitution(&binder);
    let assertion = binder.instantiate_with(&variables).unwrap();
    let TestAssertionPart::Prove(assumptions, goals) = &*assertion else {
        panic!();
    };
    (env, assumptions.clone(), goals.clone(), variables)
}

#[test]
fn declared_gat_normalization_retains_the_actual_lifetime_requirement() {
    let (env, assumptions, goals, variables) = query(
        "forall<T, 'a> { Family(T) } => {
            <<T as Family>::View<'a> as Item>::Out = &'a u32
        }",
    );
    let Wc::Predicate(Predicate::Equals(input, expected)) = goals.iter().next().unwrap() else {
        panic!();
    };
    let required: Wc = Predicate::outlives(variables[1], Lt::Static).upcast();
    let results = prove_normalize(
        declarations(),
        env.with_allow_pending_outlives(true),
        &assumptions,
        &input,
    );
    let mut matched = false;
    for (Constrained(output, c), _) in results.iter() {
        if c.known_true && output == expected {
            matched = true;
            assert!(!c.unconditionally_true(), "{c:?}");
            assert!(c.env().encloses(c.env().pending()), "{c:?}");
            assert!(c.env().pending().contains(&required), "{c:?}");
        }
    }
    assert!(matched, "{results:?}");
    assert!(!prove_normalize(declarations(), env, assumptions, input)
        .iter()
        .any(|(Constrained(_, c), _)| c.known_true));
}

#[test]
fn declared_gat_equality_retains_the_deferred_lifetime_requirement() {
    let (env, assumptions, goals, variables) = query(
        "forall<T, 'a> { Family(T) } => {
            <<T as Family>::View<'a> as Item>::Out = &'a u32
        }",
    );
    let required: Wc = Predicate::outlives(variables[1], Lt::Static).upcast();
    let results = prove(
        declarations(),
        env.with_allow_pending_outlives(true),
        &assumptions,
        &goals,
    );
    assert!(results.iter().any(|(c, _)| c.known_true), "{results:?}");
    for (c, _) in results.iter().filter(|(c, _)| c.known_true) {
        assert!(!c.unconditionally_true(), "{c:?}");
        assert!(c.env().encloses(c.env().pending()), "{c:?}");
        assert!(c.env().pending().contains(&required), "{c:?}");
    }
    assert!(!prove(declarations(), env, assumptions, goals)
        .iter()
        .any(|(c, _)| c.known_true));
}

#[test]
fn declared_gat_outlives_retains_normalization_requirements() {
    let (env, assumptions, goals, variables) = query(
        "forall<T, 'a> { Family(T) } => {
            <<T as Family>::View<'a> as Item>::Out: 'a
        }",
    );
    let required: Wc = Predicate::outlives(variables[1], Lt::Static).upcast();
    let results = prove(
        declarations(),
        env.with_allow_pending_outlives(true),
        &assumptions,
        &goals,
    );
    assert!(results
        .iter()
        .any(|(c, _)| c.known_true && c.env().pending().contains(&required)));
    for (c, _) in results.iter().filter(|(c, _)| c.known_true) {
        assert!(!c.unconditionally_true(), "{c:?}");
        assert!(c.env().encloses(c.env().pending()), "{c:?}");
    }
    assert!(!prove(declarations(), env, assumptions, goals)
        .iter()
        .any(|(c, _)| c.known_true));
}

#[test]
fn higher_ranked_declared_equality_keeps_pending_lifetimes_bound() {
    let (env, assumptions, goals, _) = query(
        "forall<T> { Family(T) } => {
            for<'a> <<T as Family>::View<'a> as Item>::Out = &'a u32
        }",
    );
    let required: Wc = term("for<'a> 'a: 'static");
    let results = prove(
        declarations(),
        env.with_allow_pending_outlives(true),
        &assumptions,
        &goals,
    );
    assert!(results.iter().any(|(c, _)| c.known_true), "{results:?}");
    for (c, _) in results.iter().filter(|(c, _)| c.known_true) {
        assert!(!c.unconditionally_true(), "{c:?}");
        assert!(c.env().encloses(c.env().pending()), "{c:?}");
        assert!(c.env().pending().contains(&required), "{c:?}");
    }
    assert!(!prove(declarations(), env, assumptions, goals)
        .iter()
        .any(|(c, _)| c.known_true));
}

#[test]
fn explicit_quantified_equality_also_keeps_pending_lifetimes_bound() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            trait Family { type View<'a>: [Item] where 'a: 'static; }
        }
    ]";
    assert!(a_mir_formality::test_program_ok(program).is_ok());
    let crates: Crates = term(program);
    let declarations = crates.to_prove_decls();
    let (env, assumptions, goals, _) = query(
        "forall<T> {
            Family(T),
            for<'a> if { 'a: 'static } <<T as Family>::View<'a> as Item>::Out = &'a u32
        } => {
            for<'b> <<T as Family>::View<'b> as Item>::Out = &'b u32
        }",
    );
    let required: Wc = term("for<'a> 'a: 'static");
    let results = prove(
        &declarations,
        env.with_allow_pending_outlives(true),
        &assumptions,
        &goals,
    );
    assert!(results.iter().any(|(c, _)| c.known_true), "{results:?}");
    for (c, _) in results.iter().filter(|(c, _)| c.known_true) {
        assert!(!c.unconditionally_true(), "{c:?}");
        assert!(c.env().encloses(c.env().pending()), "{c:?}");
        assert!(c.env().pending().contains(&required), "{c:?}");
    }
    let without_equality: Wcs = assumptions
        .iter()
        .filter(|wc| matches!(wc, Wc::Predicate(Predicate::IsImplemented(_))))
        .collect();
    assert!(!prove(
        &declarations,
        env.with_allow_pending_outlives(true),
        without_equality,
        &goals,
    )
    .iter()
    .any(|(c, _)| c.known_true));
    assert!(!prove(declarations, env, assumptions, goals)
        .iter()
        .any(|(c, _)| c.known_true));
}

#[test]
fn deferred_requirement_tracks_the_substituted_lifetime() {
    let mut env = Env::default();
    let actual = env.fresh_universal(ParameterKind::Lt);
    let inferred = env.fresh_existential(ParameterKind::Lt);
    let env = env
        .with_allow_pending_outlives(true)
        .with_pending(Predicate::outlives(inferred, Lt::Static));
    let required: Wc = Predicate::outlives(actual, Lt::Static).upcast();
    let stale: Wc = Predicate::outlives(inferred, Lt::Static).upcast();
    let results = prove(
        Program::empty(),
        env,
        (),
        Predicate::equals(inferred, actual),
    );
    assert!(results.iter().any(|(c, _)| c.known_true), "{results:?}");
    for (c, _) in results.iter().filter(|(c, _)| c.known_true) {
        assert!(c.env().encloses(c.env().pending()), "{c:?}");
        assert!(c.env().pending().contains(&required), "{c:?}");
        assert!(!c.env().pending().contains(&stale), "{c:?}");
    }
}

#[test]
fn pending_only_variables_survive_an_unrelated_goal() {
    let mut env = Env::default().with_allow_pending_outlives(true);
    let lifetime = env.fresh_universal(ParameterKind::Lt);
    let required: Wc = Predicate::outlives(lifetime, Lt::Static).upcast();
    let env = env.with_pending(&required).with_pending(&required);
    let goals: Wcs = term("{ u32 = u32 }");
    let results = prove(Program::empty(), env, (), goals);
    assert!(results.iter().any(|(c, _)| c.known_true), "{results:?}");
    for (c, _) in results.iter().filter(|(c, _)| c.known_true) {
        assert!(c.env().encloses(c.env().pending()), "{c:?}");
        assert_eq!(c.env().pending(), &[required.clone()], "{c:?}");
    }
}

#[test]
fn local_existential_pending_is_preserved_as_one_shared_witness() {
    let mut env = Env::default().with_allow_pending_outlives(true);
    let a = env.fresh_universal(ParameterKind::Lt);
    let b = env.fresh_universal(ParameterKind::Lt);
    let local = env.fresh_existential(ParameterKind::Lt);
    let c = Constraints::none(
        env.with_pending(Predicate::outlives(a, local))
            .with_pending(Predicate::outlives(local, b)),
    )
    .pop_subst(&[local]);
    assert!(!c.unconditionally_true());
    assert!(c.env().encloses(c.env().pending()), "{c:?}");
    assert_eq!(c.env().pending().len(), 1, "{c:?}");
    let expected: Binder<Wc> = term("<'a, 'b> exists<'x> { 'a: 'x, 'x: 'b }");
    let expected = expected.instantiate_with(vec![a, b]).unwrap();
    assert_eq!(c.env().pending(), &[expected], "{c:?}");
}

#[test]
fn universally_scoped_pending_keeps_its_existential_witness_inside() {
    let mut env = Env::default().with_allow_pending_outlives(true);
    let universal = env.fresh_universal(ParameterKind::Lt);
    let local = env.fresh_existential(ParameterKind::Lt);
    let c = Constraints::none(
        env.with_pending(Predicate::outlives(universal, local))
            .with_pending(Predicate::outlives(local, universal)),
    )
    .pop_forall(&[universal]);
    let expected: Wc = term("for<'a> exists<'x> { 'a: 'x, 'x: 'a }");
    assert!(!c.unconditionally_true());
    assert!(c.env().encloses(c.env().pending()), "{c:?}");
    assert_eq!(c.env().pending(), &[expected], "{c:?}");
}
