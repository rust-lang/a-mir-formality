use formality_types::{
    cast::{Downcast, Upcast},
    cast_impl,
    derive_links::UpcastFrom,
    fold::Fold,
    grammar::{InferenceVar, Parameter, Substitution, Variable},
    visit::Visit,
};

use super::env::Env;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Constraints {
    known_true: bool,
    substitution: Substitution,
}

cast_impl!(Constraints);

impl<A, B> UpcastFrom<(A, B)> for Constraints
where
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn upcast_from(term: (A, B)) -> Self {
        Constraints {
            substitution: term.upcast(),
            known_true: true,
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
        };
        c2.assert_valid();
        c2
    }
}

impl Default for Constraints {
    fn default() -> Self {
        Self {
            known_true: true,
            substitution: Default::default(),
        }
    }
}

impl Constraints {
    pub fn substitution(&self) -> &Substitution {
        &self.substitution
    }

    pub fn ambiguous(self) -> Constraints {
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
    pub fn seq(&self, c2: impl Upcast<Constraints>) -> Constraints {
        let c2: Constraints = c2.upcast();

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

    pub fn pop_subst<V>(mut self, mut env: Env, v: &[V]) -> (Env, Self)
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

impl Fold for Constraints {
    fn substitute(&self, substitution_fn: formality_types::fold::SubstitutionFn<'_>) -> Self {
        let c2 = Constraints {
            known_true: self.known_true,
            substitution: self.substitution.substitute(substitution_fn),
        };

        // not all substitutions preserve the constraint set invariant
        c2.assert_valid();

        c2
    }
}

impl Visit for Constraints {
    fn free_variables(&self) -> Vec<Variable> {
        let Constraints {
            known_true: _,
            substitution,
        } = self;

        substitution.free_variables().into_iter().collect()
    }

    fn size(&self) -> usize {
        let Constraints {
            known_true: _,
            substitution,
        } = self;
        substitution.size()
    }

    fn assert_valid(&self) {
        let Constraints {
            known_true: _,
            substitution,
        } = self;

        let domain = substitution.domain();

        for &v in &domain {
            assert!(v.downcast::<InferenceVar>().is_some());
            assert!(v.is_free());
            assert!(is_valid_binding(v, &substitution[v]));
        }

        let range = substitution.range();
        range
            .iter()
            .for_each(|t| assert!(domain.iter().all(|&v| !occurs_in(v, t))));
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
        known_true: true,
        substitution: Substitution::default(),
    }
}
