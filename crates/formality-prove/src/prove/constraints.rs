use formality_types::{
    cast::Upcast,
    cast_impl,
    derive_links::UpcastFrom,
    fold::Fold,
    grammar::{Parameter, Substitution, Variable},
    term::Term,
    visit::Visit,
};

use super::env::Env;

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
        let c2 = Constraints {
            substitution,
            known_true: true,
            result: (),
        };
        c2.assert_valid();
        c2
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

    /// Given constraings from solving the subparts of `(A /\ B)`, yield combined constraints.
    ///
    /// # Parameters
    ///
    /// * `self` -- the constraints from solving `A`
    /// * `c2` -- the constraints from solving `B` (after applying substitution from `self` to `B`)
    pub fn seq<R2: Term>(&self, c2: impl Upcast<Constraints<R2>>) -> Constraints<R2>
    where
        R: CombineResults<R2>,
    {
        let c2: Constraints<R2> = c2.upcast();

        self.assert_valid();
        c2.assert_valid();

        // This substitution should have already been applied to produce
        // `c2`, therefore we don't expect any bindings for *our* variables.
        assert!(self
            .substitution
            .domain()
            .is_disjoint(&c2.substitution.domain()));

        // Similarly, we don't expect any references to variables that we have
        // defined.
        assert!(self
            .substitution
            .domain()
            .iter()
            .all(|v| !occurs_in(v, &c2.substitution)));

        // Apply c2's substitution to our substitution (since it may have bound
        // existential variables that we reference)
        let c1_substitution = c2.substitution.apply(&self.substitution);

        Constraints {
            known_true: self.known_true && c2.known_true,
            result: R::combine(&self.result, c2.result),
            substitution: c1_substitution.into_iter().chain(c2.substitution).collect(),
        }
    }

    pub fn assert_valid_in(&self, env: &Env) {
        self.assert_valid();

        // Each variable `x` is only bound to a term of strictly lower universe.
        // This implies that `x` does not appear in `p`.
        for (x, p) in self.substitution.iter() {
            let fvs = p.free_variables();
            fvs.iter()
                .for_each(|fv| assert!(env.universe(fv) < env.universe(x)));
        }
    }

    pub fn pop_subst<V>(mut self, mut env: Env, v: &[V]) -> (Env, Constraints<R>)
    where
        V: Upcast<Variable> + Copy,
    {
        self.assert_valid_in(&env);

        if v.len() == 0 {
            return (env, self);
        }

        let vars = env.pop_vars(v);
        self.substitution -= vars;

        (env, self)
    }
}

impl<R: Term> Fold for Constraints<R> {
    fn substitute(&self, substitution_fn: formality_types::fold::SubstitutionFn<'_>) -> Self {
        let c2 = Constraints {
            known_true: self.known_true,
            substitution: self.substitution.substitute(substitution_fn),
            result: self.result.substitute(substitution_fn),
        };

        // not all substitutions preserve the constraint set invariant
        c2.assert_valid();

        c2
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

pub fn no_constraints() -> Constraints {
    Constraints {
        result: (),
        known_true: true,
        substitution: Substitution::default(),
    }
}

pub trait CombineResults<R> {
    fn combine(r0: &Self, r1: R) -> R;
}

impl<R> CombineResults<R> for () {
    fn combine((): &(), r1: R) -> R {
        r1
    }
}

impl CombineResults<Vec<Parameter>> for Parameter {
    fn combine(r0: &Self, mut r1: Vec<Parameter>) -> Vec<Parameter> {
        r1.push(r0.clone());
        r1
    }
}
