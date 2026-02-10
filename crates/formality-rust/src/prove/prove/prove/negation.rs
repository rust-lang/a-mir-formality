use crate::prove::prove::{Bias, Constraints, Env};
use formality_core::{fold::CoreFold, judgment::ProofTree, ProvenSet, Upcast};
use crate::types::{
    grammar::{Substitution, Variable, Wcs},
    rust::FormalityLang,
};

/// This succeeds if `f` definitely fails: there are no possible
/// ways to prove its body.
///
/// This fails if `f' is ambiguous, so it is biases towards soundness.
/// It never succeeds if `f` may actually hold, but may fail even if
/// `f` does not hold. For this to be correct the body of `f` must
/// be complete: it must only fail if there is definitely no solution.
///
/// Proving `∀X. not(F(X))` is the same as proving `not(∃X. F(X))`, so
/// this replaces all universal variables with existantials.
#[tracing::instrument(level = "Debug", skip_all)]
pub fn is_definitely_not_proveable<T: CoreFold<FormalityLang>>(
    env: &Env,
    assumptions: impl Upcast<Wcs>,
    data: T,
    f: impl FnOnce(Env, Wcs, T) -> ProvenSet<Constraints>,
) -> ProvenSet<Constraints> {
    assert!(env.bias() == Bias::Soundness);

    // Require only universal variables, so e.g. `forall<T> not (T: Foo)`.
    // For those, we can convert universal and existential and try to prove
    // the inverse (so try to prove `exists<T> (T: Foo)`, with a bias towards
    // completeness. If that fails, then we know that for all `T`, `T: Foo` is false.
    //
    // But with existential we can do no such trick. Given `exists<T> not (T: Foo)`,
    // certainly if we can prove `forall<T> (T: Foo)` then we know this is false.
    // But if we can't prove it, it doesn't tell us that there exists a `T`
    // where `not (T: Foo)`, only that we could not prove it is true for all `T`
    // (put another way, it may still be true for all T, we just couldn't prove it).
    //
    // FIXME(nikomatsakis, #227): This sounds suspicious, if we *truly* handle bias correctly,
    // this might not hold; I suspect we do not, which is why I wonder if we can
    // frame this another way, such as "known not to hold".)
    assert!(env.only_universal_variables());
    negation_via_failure(env, assumptions, data, f)
}

/// This fails if `f` definitely succeeds. There is no way to prove `f`.
/// This should only be used if we're currently biased towards completeness
/// as it returns success even if there's still ambiguity.
#[tracing::instrument(level = "Debug", skip_all)]
pub fn may_not_be_provable<T: CoreFold<FormalityLang>>(
    env: &Env,
    assumptions: impl Upcast<Wcs>,
    data: T,
    f: impl FnOnce(Env, Wcs, T) -> ProvenSet<Constraints>,
) -> ProvenSet<Constraints> {
    assert!(env.bias() == Bias::Completeness);
    negation_via_failure(env, assumptions, data, f)
}

pub fn negation_via_failure<T: CoreFold<FormalityLang>>(
    env: &Env,
    assumptions: impl Upcast<Wcs>,
    data: T,
    f: impl FnOnce(Env, Wcs, T) -> ProvenSet<Constraints>,
) -> ProvenSet<Constraints> {
    let assumptions: Wcs = assumptions.upcast();
    tracing::debug!(?assumptions, ?data);

    let flipped_bias = match env.bias() {
        Bias::Soundness => Bias::Completeness,
        Bias::Completeness => Bias::Soundness,
    };

    // As we prove the negation, we have to flip the bias and
    // quantifiers of all variables.
    let mut flipped_env = Env::new_with_bias(flipped_bias);
    let flip_quantification: Substitution = env
        .variables()
        .iter()
        .map(|v| {
            let v1: Variable = if v.is_universal() {
                flipped_env.fresh_existential(v.kind()).upcast()
            } else {
                flipped_env.fresh_universal(v.kind()).upcast()
            };
            (v, v1)
        })
        .collect();

    let flipped_assumptions = flip_quantification.apply(&assumptions);
    let flipped_data = flip_quantification.apply(&data);

    let cs = f(flipped_env, flipped_assumptions, flipped_data);
    match cs.into_map() {
        Ok(s) => {
            if let Some((constraints, _tree)) = s
                .iter()
                .find(|(constraints, _)| constraints.unconditionally_true())
            {
                ProvenSet::failed(
                    "negation_via_failure",
                    format!("found an unconditionally true solution {constraints:?}"),
                )
            } else {
                tracing::debug!("ambiguous `negation_via_failure`, solutions: {s:?}");
                // Ambiguous - grab a tree from one of the results
                let (_, sample_tree) = s.iter().next().unwrap();
                let result = Constraints::none(env).ambiguous();
                ProvenSet::singleton((
                    result.clone(),
                    ProofTree::new(
                        format!("{:?}", result),
                        Some("ambiguous_negation"),
                        vec![sample_tree.clone()],
                    ),
                ))
            }
        }

        Err(err) => {
            tracing::debug!("Proved `negation_via_failure`, error = {err}");
            // Negation succeeded because f failed
            let result = Constraints::none(env);
            ProvenSet::singleton((
                result.clone(),
                ProofTree::leaf(format!("negation succeeded: {}", err)),
            ))
        }
    }
}
