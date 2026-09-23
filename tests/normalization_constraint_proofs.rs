use formality_core::Upcast;
use formality_rust::grammar::{Crates, Lt, Parameter, Predicate, Wc, Wcs};
use formality_rust::prove::test_util::TestAssertionPart;
use formality_rust::prove::{prove, Env, Program};
use formality_rust::rust::term;

fn declarations() -> Program {
    let crates: Crates = term(
        "[crate test {
            trait Family { type View<'a>: []; }
            trait Item { type Out: []; }
        }]",
    );
    let _ = formality_rust::check::check_all_crates(&crates)
        .check_proven()
        .unwrap();
    crates.to_prove_decls()
}

#[test]
fn a_normalization_substitution_updates_the_next_edges_guard() {
    let TestAssertionPart::ForAll(binder) = term(
        "forall<C, D, 'actual> exists<'inferred> {
            Family(C), Item(D),
            <C as Family>::View<'actual> = <D as Item>::Out,
            if { 'inferred: 'static } <D as Item>::Out = u32
        } => { <C as Family>::View<'inferred> = u32 }",
    ) else {
        panic!();
    };
    let (env, universals) = Env::default().universal_substitution(&binder);
    let assertion = binder.instantiate_with(&universals).unwrap();
    let TestAssertionPart::Exists(binder) = &*assertion else {
        panic!();
    };
    let (env, existentials) = env.existential_substitution(binder);
    let assertion = binder.instantiate_with(&existentials).unwrap();
    let TestAssertionPart::Prove(assumptions, goals) = &*assertion else {
        panic!();
    };
    let actual: Parameter = universals[2].upcast();
    let inferred: Parameter = existentials[0].upcast();
    let required: Wc = Predicate::outlives(&actual, Lt::Static).upcast();

    let deferred = prove(
        declarations(),
        env.with_allow_pending_outlives(true),
        assumptions,
        goals,
    );
    assert!(deferred.iter().any(|(c, _)| c.known_true));
    for (c, _) in deferred.iter().filter(|(c, _)| c.known_true) {
        assert_eq!(c.substitution().apply(&inferred), actual);
        assert!(c.env().encloses(c.env().pending()));
        assert_eq!(c.env().pending(), &[required.clone()]);
    }

    assert!(!prove(declarations(), &env, assumptions, goals)
        .iter()
        .any(|(c, _)| c.known_true));
    let justified = prove(declarations(), env, (assumptions, &required), goals);
    assert!(justified.iter().any(|(c, _)| {
        c.known_true && c.env().pending().is_empty() && c.substitution().apply(&inferred) == actual
    }));
}

#[test]
fn equal_normalized_outputs_retain_independent_pending_conditions() {
    let TestAssertionPart::ForAll(binder) = term(
        "forall<C, D, 'a, 'b> {
            Family(C), Item(D),
            <C as Family>::View<'static> = <D as Item>::Out,
            if { 'a: 'static } <D as Item>::Out = u32,
            if { 'b: 'static } <D as Item>::Out = u32
        } => { <C as Family>::View<'static> = u32 }",
    ) else {
        panic!();
    };
    let (env, universals) = Env::default().universal_substitution(&binder);
    let assertion = binder.instantiate_with(&universals).unwrap();
    let TestAssertionPart::Prove(assumptions, goals) = &*assertion else {
        panic!();
    };
    let a: Wc = Predicate::outlives(universals[2], Lt::Static).upcast();
    let b: Wc = Predicate::outlives(universals[3], Lt::Static).upcast();
    let deferred = prove(
        declarations(),
        env.with_allow_pending_outlives(true),
        assumptions,
        goals,
    );
    for required in [&a, &b] {
        assert!(deferred
            .iter()
            .any(|(c, _)| c.known_true && c.env().pending() == &[required.clone()]));
    }
    for (c, _) in deferred.iter().filter(|(c, _)| c.known_true) {
        assert!(c.substitution().is_empty());
        assert!(c.env().encloses(c.env().pending()));
        assert!(!c.env().pending().is_empty());
        assert!(c.env().pending().iter().all(|wc| wc == &a || wc == &b));
    }

    assert!(!prove(declarations(), &env, assumptions, goals)
        .iter()
        .any(|(c, _)| c.known_true));
    for required in [a, b] {
        let assumptions: Wcs = (assumptions, required).upcast();
        assert!(prove(declarations(), &env, assumptions, goals)
            .iter()
            .any(|(c, _)| c.unconditionally_true()));
    }
}
