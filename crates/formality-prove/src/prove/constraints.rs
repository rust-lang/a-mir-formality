use formality_types::{
    cast::Upcast,
    cast_impl,
    derive_links::UpcastFrom,
    fold::Fold,
    grammar::{Binder, InferenceVar, Parameter, Substitution, Variable},
    term::Term,
    visit::Visit,
};

use super::subst::existential_substitution;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Constraints<R: Term = ()> {
    result: R,
    known_true: bool,
    substitution: Substitution,
}

cast_impl!(impl(R: Term) Constraints<R>);

impl<A, B> UpcastFrom<(A, B)> for Constraints
where
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn upcast_from(term: (A, B)) -> Self {
        Constraints {
            substitution: term.upcast(),
            known_true: true,
            result: (),
        }
    }
}

impl<A, B> FromIterator<(A, B)> for Constraints
where
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        let substitution = iter.into_iter().collect();
        let c = Constraints {
            substitution,
            known_true: true,
            result: (),
        };
        c.assert_valid();
        c
    }
}

impl Default for Constraints {
    fn default() -> Self {
        Self {
            result: (),
            known_true: true,
            substitution: Default::default(),
        }
    }
}

impl<R: Term> Constraints<R> {
    pub fn substitution(&self) -> &Substitution {
        &self.substitution
    }

    pub fn ambiguous(self) -> Constraints<R> {
        Self {
            known_true: false,
            ..self
        }
    }

    pub fn map<S: Term>(self, op: impl FnOnce(R) -> S) -> Constraints<S> {
        let Constraints {
            result,
            known_true,
            substitution,
        } = self;
        let result = op(result);
        Constraints {
            known_true,
            substitution,
            result,
        }
    }

    pub fn split_result(self) -> (R, Constraints) {
        let Constraints {
            result,
            known_true,
            substitution,
        } = self;
        (
            result,
            Constraints {
                known_true,
                substitution,
                result: (),
            },
        )
    }
}

pub fn instantiate_and_apply_constraints<T: Term, R: Term>(
    c: Binder<Constraints<R>>,
    term: T,
) -> (Vec<InferenceVar>, Constraints<R>, T) {
    let existentials = existential_substitution(&c, &term);
    let c = c.instantiate_with(&existentials).unwrap();
    let term = c.substitution().apply(&term);
    (existentials, c, term)
}

pub fn merge_constraints<R0: Term, R1: Term>(
    existentials: impl Upcast<Vec<Variable>>,
    c0: impl Upcast<Constraints<R0>>,
    c1: Binder<Constraints<R1>>,
) -> Binder<Constraints<R1>>
where
    R0: CombineResults<R1>,
{
    let c0: Constraints<R0> = c0.upcast();
    c0.assert_valid();

    let (c1_bound_vars, c1) = c1.open();
    c1.assert_valid();

    assert!(c0
        .substitution
        .domain()
        .is_disjoint(&c1.substitution.domain()));
    assert!(!c0
        .substitution
        .domain()
        .iter()
        .any(|v| occurs_in(v, &c1.substitution)));

    let existentials: Vec<Variable> = existentials.upcast();

    let c0 = c1.substitution.apply(&c0);

    // Drop any bindings `X := P` from the subst that appear in the `variables` set;
    // those are existentials that we temporarily introduced and no longer need.
    let substitution = c0
        .substitution
        .into_iter()
        .chain(c1.substitution)
        .filter(|(v, _)| !existentials.contains(&v.upcast()))
        .collect();

    let known_true = c0.known_true && c1.known_true;

    let result = R0::combine(c0.result, c1.result);

    Binder::mentioned(
        (c1_bound_vars, existentials),
        Constraints {
            known_true,
            substitution,
            result,
        },
    )
}

impl<R: Term> Fold for Constraints<R> {
    fn substitute(&self, substitution_fn: formality_types::fold::SubstitutionFn<'_>) -> Self {
        let c = Constraints {
            known_true: self.known_true,
            substitution: self.substitution.substitute(substitution_fn),
            result: self.result.substitute(substitution_fn),
        };

        // not all substitutions preserve the constraint set invariant
        c.assert_valid();

        c
    }
}

impl<R: Term> Visit for Constraints<R> {
    fn free_variables(&self) -> Vec<Variable> {
        let Constraints {
            known_true: _,
            substitution,
            result,
        } = self;

        substitution
            .free_variables()
            .into_iter()
            .chain(result.free_variables())
            .collect()
    }

    fn size(&self) -> usize {
        let Constraints {
            known_true: _,
            substitution,
            result,
        } = self;
        substitution.size() + result.size()
    }

    fn assert_valid(&self) {
        let Constraints {
            known_true: _,
            substitution,
            result,
        } = self;

        let domain = substitution.domain();

        for &v in &domain {
            assert!(v.is_free());
            assert!(is_valid_binding(v, &substitution[v]));
        }

        let range = substitution.range();
        range
            .iter()
            .for_each(|t| assert!(domain.iter().all(|&v| !occurs_in(v, t))));

        result.assert_valid();
    }
}

pub fn occurs_in(v: impl Upcast<Variable>, t: &impl Visit) -> bool {
    let v: Variable = v.upcast();
    t.free_variables().contains(&v)
}

/// True if `t` would be a valid binding for `v`, meaning...
/// * `v` does not appear in `t`; and,
/// * all free variables in `t` are within the universe of `v`
pub fn is_valid_binding(v: impl Upcast<Variable>, t: &impl Visit) -> bool {
    let v: Variable = v.upcast();
    assert!(v.is_free());

    let v_universe = v.max_universe();
    t.free_variables()
        .into_iter()
        .all(|fv| fv != v && fv.max_universe() <= v_universe)
}

pub fn constrain(v: impl Upcast<InferenceVar>, p: impl Upcast<Parameter>) -> Binder<Constraints> {
    let v: InferenceVar = v.upcast();
    let p: Parameter = p.upcast();
    Binder::dummy((v, p).upcast())
}

pub fn no_constraints<R: Term>(result: R) -> Binder<Constraints<R>> {
    Binder::dummy(Constraints {
        result,
        known_true: true,
        substitution: Substitution::default(),
    })
}

pub trait CombineResults<R> {
    fn combine(r0: Self, r1: R) -> R;
}

impl<R> CombineResults<R> for () {
    fn combine((): (), r1: R) -> R {
        r1
    }
}

impl CombineResults<Vec<Parameter>> for Parameter {
    fn combine(r0: Self, mut r1: Vec<Parameter>) -> Vec<Parameter> {
        r1.push(r0);
        r1
    }
}
