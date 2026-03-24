use super::env::Env;
use crate::grammar::{ExistentialVar, Parameter, Substitution, Variable};
use crate::rust::Visit;
use formality_core::{cast_impl, visit::CoreVisit, Downcast, Upcast, UpcastFrom};

/// A wrapper around a value that is constrained by a set of conditions.
///
/// We sometimes use `Constrained<()>` as a "upcast-able synonym" for `Constraints`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Constrained<T, C = Constraints>(pub T, pub C);

impl<T> Constrained<T> {
    pub fn none(env: impl Upcast<Env>, value: T) -> Self {
        Self(value, Constraints::none(env))
    }

    pub fn constraints(&self) -> &Constraints {
        &self.1
    }

    pub fn unconditionally_true(&self) -> bool {
        self.constraints().unconditionally_true()
    }
}

impl<C> UpcastFrom<Constrained<(), C>> for Constraints
where
    C: Upcast<Constraints>,
{
    fn upcast_from(Constrained((), c): Constrained<(), C>) -> Self {
        c.upcast()
    }
}

impl UpcastFrom<Constraints> for Constrained<()> {
    fn upcast_from(c: Constraints) -> Self {
        Constrained((), c)
    }
}

impl<T: Clone, U, C, D> UpcastFrom<Constrained<T, C>> for Constrained<U, D>
where
    T: Upcast<U>,
    C: Upcast<D>,
{
    fn upcast_from(Constrained(t, c): Constrained<T, C>) -> Self {
        Constrained(t.upcast(), c.upcast())
    }
}

/// Captures the conditions under which something is true.
///
/// Examples:
///
/// * The `substitution` indicates equality constraints for existential (inference) variables.
///   E.g. if you try to prove `Vec<?A> = Vec<u32>`, the result will be "True; [?A => u32]".
///   This substitution is then to be applied to any future goals/terms that may reference `?A`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Constraints {
    /// The environment in which future goals should be proven.
    /// This is an extension of the original environment; it may contain
    /// new variables and new pending proof obligations.
    pub env: Env,

    /// If this is false, it indicates that the result is actually *ambiguous* and not known to be true.
    /// This occurs when e.g. we hit overflow or some other condition in which we can neither prove the
    /// result true nor false.
    pub known_true: bool,

    /// Maps an existential variable to the value which it must be equal to.
    /// For example `Vec<?A> = Vec<u32>` would result in a substitution `[?A => u32]`.`
    pub substitution: Substitution,
}

cast_impl!(Constraints);

impl<E, A, B> UpcastFrom<(E, (A, B))> for Constraints
where
    E: Upcast<Env>,
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn upcast_from((env, pair): (E, (A, B))) -> Self {
        Constraints::from(env, std::iter::once(pair))
    }
}

impl Constraints {
    pub fn none(env: impl Upcast<Env>) -> Self {
        let v: Vec<(Variable, Parameter)> = vec![];
        Self::from(env, v)
    }

    pub fn unconditionally_true(&self) -> bool {
        self.known_true && self.substitution.is_empty() && self.env.pending().is_empty()
    }

    /// Construct a set of constraints from a set of substitutions and the previous environment.
    pub fn from(
        env: impl Upcast<Env>,
        iter: impl IntoIterator<Item = (impl Upcast<Variable>, impl Upcast<Parameter>)>,
    ) -> Self {
        let env = env.upcast();
        let substitution: Substitution = iter.into_iter().collect();
        assert!(env.encloses(substitution.range()));
        assert!(env.encloses(substitution.domain()));
        let c2 = Constraints {
            env,
            substitution,
            known_true: true,
        };
        c2.assert_valid();
        c2
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    pub fn substitution(&self) -> &Substitution {
        &self.substitution
    }

    pub fn ambiguous(self) -> Constraints {
        Self {
            known_true: false,
            ..self
        }
    }

    /// Given constraints from solving the subparts of `(A /\ B)`, yield combined constraints.
    ///
    /// # Parameters
    ///
    /// * `self` -- the constraints from solving `A`
    /// * `c2` -- the constraints from solving `B` (after applying substitution from `self` to `B`)
    pub fn seq(&self, c2: impl Upcast<Constraints>) -> Constraints {
        let c2: Constraints = c2.upcast();

        tracing::debug!("Constraints::seq({self:?}, {c2:?}");

        self.assert_valid();
        c2.assert_valid();
        assert!(c2.is_valid_extension_of(&self.env));

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
            env: c2.env,
            known_true: self.known_true && c2.known_true,
            substitution: c1_substitution.into_iter().chain(c2.substitution).collect(),
        }
    }

    /// Given a set of variables `v` created via [`Env::instantiate_universally`][]
    /// or [`Env::instantiate_existentially`][], removes `v` and all variables created *since* `v`
    /// from the environment and from the substitution.
    pub fn pop_subst<V>(&self, v: &[V]) -> Self
    where
        V: Upcast<Variable> + Copy,
    {
        let mut env = self.clone();

        if v.is_empty() {
            return env.clone();
        }

        let vars = env.env.pop_vars(v);
        env.substitution -= vars;

        env
    }

    pub fn is_valid_extension_of(&self, env0: &Env) -> bool {
        self.env.is_valid_extension_of(env0)
    }
}

impl CoreVisit<crate::prove::prove::FormalityLang> for Constraints {
    fn free_variables(&self) -> Vec<Variable> {
        let Constraints {
            env,
            known_true: _,
            substitution,
        } = self;

        // Debatable if `env.free_variables()` should be considered
        // free here, env kind of acts as a binder. Don't think it matters.

        env.free_variables()
            .into_iter()
            .chain(substitution.free_variables())
            .collect()
    }

    fn size(&self) -> usize {
        let Constraints {
            env,
            known_true: _,
            substitution,
        } = self;
        env.size() + substitution.size()
    }

    fn assert_valid(&self) {
        let Constraints {
            env,
            known_true: _,
            substitution,
        } = self;

        let domain = substitution.domain();
        let range = substitution.range();

        assert!(env.encloses(&domain));
        assert!(env.encloses(&range));

        // No variable in the domain appears in any part of the range;
        // this prevents the obvious occurs check violations like `X = Vec<X>`
        // but also indirect ones like `X = Vec<Y>, Y = X`; it also implies that
        // the substitution has been fully applied, so we don't have `X = Vec<Y>, Y = u32`.
        range
            .iter()
            .for_each(|p| assert!(domain.iter().all(|&v| !occurs_in(v, p))));

        // Each variable `x` is only bound to a term of strictly lower universe.
        // This implies that `x` does not appear in `p`.
        for (x, p) in substitution.iter() {
            assert!(x.is_a::<ExistentialVar>());

            assert!(!occurs_in(x, &p));

            let fvs = p.free_variables();
            fvs.iter()
                .for_each(|fv| assert!(env.universe(fv) < env.universe(x)));
        }
    }
}

pub fn occurs_in(v: impl Upcast<Variable>, t: &impl Visit) -> bool {
    let v: Variable = v.upcast();
    t.free_variables().contains(&v)
}
