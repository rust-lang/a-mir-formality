use crate::grammar::Parameter;
use crate::grammar::{
    Binder, ExistentialVar, ParameterKind, Substitution, UniversalVar, VarIndex, VarSubstitution,
    Variable, Wc,
};
use crate::prove::prove::prove::constraints::occurs_in;
use crate::rust::{Fold, Visit};
use formality_core::set;
use formality_core::{cast_impl, visit::CoreVisit, Set, To, Upcast, UpcastFrom};
use formality_macros::term;

/// Whether the solver has to be sound or complete. While it should
/// ideally be both at all times, we have some rules which break these
/// properties, so we have to make sure they are only used while in
/// the proper `Env`. Regardless of the current bias, it's always valid
/// to return an ambiguous result.
#[derive(Default, Debug, Clone, Copy, Hash, Ord, Eq, PartialEq, PartialOrd)]
pub enum Bias {
    /// Whenever we're able to prove something, it has to definitely be
    /// *truly true*. We must never be able to prove somethingwhich is not
    /// *truly true*. This is the default unless we're currently trying to
    /// prove the negation via "negation as failure".
    ///
    /// By convention, functions which may only be used with a bias
    /// for soundness are called `fn is_X` or `fn is_definitely_X`.
    #[default]
    Soundness,
    /// If we are unable to find a solution for some goal, this goal must
    /// definitely not be proveable, it is known to not be *truly true*. This
    /// implies that we must only add constraints if they are definitely
    /// necessary as we can otherwise use these constraints to incorrectly
    /// disprove something.
    ///
    /// Most notably, this is used by coherence checking to make sure that impls
    /// definitely do not overlap.
    ///
    /// By convention, functions which may only be used with a bias for
    /// completeness are called `fn may_X`.
    Completeness,
}

#[derive(Default, Debug, Clone, Hash, Ord, Eq, PartialEq, PartialOrd)]
pub struct Env {
    /// The set of variables that are in scope (universal and existential).
    /// All terms should only reference free variables found in this set.
    pub variables: Vec<Variable>,

    /// Maps an existential variable to the value which it must be equal to.
    /// For example `Vec<?A> = Vec<u32>` would result in a substitution `[?A => u32]`.`
    pub substitution: Substitution,

    /// XXX this needs to be explained
    pub bias: Bias,

    /// If this is false, it indicates that the result is actually *ambiguous* and not known to be true.
    /// This occurs when e.g. we hit overflow or some other condition in which we can neither prove the
    /// result true nor false.
    pub known_true: bool,

    /// Pending goals that have not yet been proven.
    /// When we prove a conjunction `(A && B)`, proving `A` may result in
    /// some pending items (e.g., subtype constraints).
    ///
    /// These are then added into the env when we prove `B` so that if `B`
    /// winds up constraining an inference varibale, it can also try to prove
    /// the pending constraints that are now unlocked.
    ///
    /// Whenever a "successful" proof results, the pending obligations
    pub pending: Vec<Wc>,

    /// When true, outlives constraints like `'a: 'b` can be deferred as pending
    /// obligations rather than being proven immediately. This is used during
    /// type checking to accumulate constraints that will be verified by the
    /// borrow checker. When false, outlives must be proven from assumptions.
    pub allow_pending_outlives: bool,
}

impl Env {
    pub fn new_with_bias(bias: Bias) -> Self {
        Env {
            known_true: true,
            variables: Default::default(),
            bias,
            substitution: Default::default(),
            pending: vec![],
            allow_pending_outlives: false,
        }
    }

    pub fn add_substitution(&self, var: impl Upcast<Variable>, ty: impl Upcast<Parameter>) -> Self {
        let mut this = self.clone();
        this.substitution.insert(var, ty);
        this
    }

    pub fn add_all_substitutions(&self, subst: Substitution) -> Self {
        let this = self.clone();
        for (var, ty) in subst {
            this.add_substitution(var, ty);
        }
        this
    }

    pub fn only_universal_variables(&self) -> bool {
        self.variables.iter().all(|v| v.is_universal())
    }

    pub fn bias(&self) -> Bias {
        self.bias
    }

    pub fn known_true(&self) -> bool {
        self.known_true
    }

    pub fn substitution(&self) -> &Substitution {
        &self.substitution
    }

    pub fn allow_pending_outlives(&self) -> bool {
        self.allow_pending_outlives
    }

    /// Return a clone of the environment with `allow_pending_outlives` set to the given value
    pub fn with_allow_pending_outlives(&self, allow: bool) -> Self {
        let mut env = self.clone();
        env.allow_pending_outlives = allow;
        env
    }

    /// Return a clone of the environment with `w` as a pending where-clause
    pub fn with_pending(&self, w: impl Upcast<Wc>) -> Self {
        let mut env = self.clone();
        env.pending.push(w.upcast());
        env
    }

    /// Given a set of variables `v` created via [`Env::instantiate_universally`][]
    /// or [`Env::instantiate_existentially`][], removes `v` and all variables created *since* `v`
    /// from the environment and from the substitution.
    pub fn pop_subst<V>(&mut self, v: &[V]) -> Self
    where
        V: Upcast<Variable> + Copy,
    {
        if v.is_empty() {
            return self.clone();
        }

        let vars = self.pop_vars(v);
        self.substitution -= vars;

        self.clone()
    }

    pub fn unconditionally_true(&self) -> bool {
        self.known_true && self.substitution.is_empty() && self.pending.is_empty()
    }

    /// Given constraints from solving the subparts of `(A /\ B)`, yield combined constraints.
    ///
    /// # Parameters
    ///
    /// * `self` -- the constraints from solving `A`
    /// * `c2` -- the constraints from solving `B` (after applying substitution from `self` to `B`)
    /// TODO: revisit this again
    pub fn seq(&self, c2: &Env) -> Self {
        tracing::debug!("Constraints::seq({self:?}, {c2:?}");

        self.assert_valid();
        c2.assert_valid();
        assert!(c2.is_valid_extension_of(&self));

        // This substitution should have already been applied to produce
        // `c2`, therefore we don't expect any bindings for *our* variables.
        assert!(self
            .substitution()
            .domain()
            .is_disjoint(&c2.substitution().domain()));

        // Similarly, we don't expect any references to variables that we have
        // defined.
        assert!(self
            .substitution()
            .domain()
            .iter()
            .all(|v| !occurs_in(v, &c2.substitution())));

        // Apply c2's substitution to our substitution (since it may have bound
        // existential variables that we reference)
        let c1_substitution = c2.substitution().apply(&self.substitution());

        let new_subst = c1_substitution
            .into_iter()
            .chain(c2.substitution().clone().into_iter())
            .collect();
        let new_known_true = self.known_true && c2.known_true;

        Env {
            known_true: new_known_true,
            substitution: new_subst,
            ..c2.clone()
        }
    }

    pub fn ambiguous(&self) -> Self {
        Env {
            known_true: false,
            ..self.clone()
        }
    }
}

cast_impl!(Env);

#[term(U($index))]
#[derive(Copy, Default)]
pub struct Universe {
    index: usize,
}

/// Maximum universe of a variable in an environment or a term.
///
/// This is isomorphic to `Option<Universe>` but (a) conveys semantic
/// meaning and (b) has the correct ordering (no universe < some universe).
#[term]
#[derive(Copy)]
pub enum MaxUniverse {
    /// No universes in the variable or term. This is the *least* value.
    None,

    /// Maximum universe that appears. This is greater.
    Some(Universe),
}

impl MaxUniverse {
    pub fn contains(self, u: Universe) -> bool {
        match self {
            MaxUniverse::None => false,
            MaxUniverse::Some(max) => u <= max,
        }
    }
}

impl Default for MaxUniverse {
    fn default() -> Self {
        MaxUniverse::None
    }
}

impl Env {
    /// Maximum universe of any free variable in this environment (or None if there are no free variables).
    pub fn max_universe(&self) -> MaxUniverse {
        if self.variables.is_empty() {
            MaxUniverse::None
        } else {
            MaxUniverse::Some(Universe {
                index: self.variables.len(),
            })
        }
    }

    /// A variable's *universe* in the environment determines what
    /// other variables it can be related to. For example, an
    /// existential variable in universe U1 can be equated to variables
    /// in universes U0 < U1.
    pub fn universe(&self, v: impl Upcast<Variable>) -> Universe {
        let v: Variable = v.upcast();
        assert!(self.variables.contains(&v));
        Universe {
            index: self.variables.iter().position(|e| *e == v).unwrap(),
        }
    }

    fn fresh_index(&self) -> VarIndex {
        let max_index = self
            .variables
            .iter()
            .map(|v| match v {
                Variable::UniversalVar(pv) => pv.var_index.index,
                Variable::ExistentialVar(iv) => iv.var_index.index,
                Variable::BoundVar(_) => panic!(),
            })
            .max()
            .unwrap_or(0);

        VarIndex {
            index: max_index + 1,
        }
    }

    pub fn fresh_existential(&mut self, kind: ParameterKind) -> ExistentialVar {
        let var_index = self.fresh_index();
        let v = ExistentialVar { kind, var_index };
        self.variables.push(v.upcast());
        v
    }

    pub fn fresh_universal(&mut self, kind: ParameterKind) -> UniversalVar {
        let var_index = self.fresh_index();
        let v = UniversalVar { kind, var_index };
        self.variables.push(v.upcast());
        v
    }

    pub fn insert_fresh_before(&mut self, kind: ParameterKind, rank: Universe) -> ExistentialVar {
        let var_index = self.fresh_index();
        let v = ExistentialVar { kind, var_index };
        self.variables.insert(rank.index, v.upcast());
        v
    }

    pub fn order_by_universe<V>(&self, v1: V, v2: V) -> (V, V)
    where
        V: Upcast<Variable> + Copy,
    {
        let r1 = self.universe(v1);
        let r2 = self.universe(v2);
        if r1 < r2 {
            (v1, v2)
        } else {
            (v2, v1)
        }
    }

    /// Pending goals that must still be proven
    pub fn pending(&self) -> &[Wc] {
        &self.pending
    }

    /// An env A *encloses* a term `v` if all free variables in `v`
    /// are defined in A.
    pub fn encloses(&self, v: impl Visit) -> bool {
        v.free_variables()
            .into_iter()
            .all(|v| self.variables.contains(&v))
    }

    /// An env A is a *valid extension* of an env B if
    /// it contains a superset of the variables in B and
    /// it respects the ordering of all variables in B.
    pub fn is_valid_extension_of(&self, env0: &Env) -> bool {
        self.encloses(env0)
            && (1..env0.variables.len()).all(|i| {
                let v0_a = env0.variables[i - 1];
                let v0_b = env0.variables[i];
                self.universe(v0_a) < self.universe(v0_b)
            })
    }

    fn fresh_substitution<V>(
        &mut self,
        kinds: &[ParameterKind],
        v: impl Fn(ParameterKind, VarIndex) -> V,
    ) -> Vec<V>
    where
        V: Upcast<Variable>,
    {
        let fresh_index = self.fresh_index();
        let vars: Vec<_> = kinds
            .iter()
            .zip(0..)
            .map(|(&kind, offset)| v(kind, fresh_index + offset))
            .collect();
        self.variables
            .extend(vars.iter().map(|v| v.to::<Variable>()));
        vars
    }

    /// Returns `(e,s)` where
    ///
    /// * `s` is a list of fresh universal variables for each variable bound in `b`; and,
    /// * `e` is an extended environment with these universal variables in scope.
    pub fn universal_substitution<T>(&self, b: &Binder<T>) -> (Env, Vec<UniversalVar>)
    where
        T: Fold,
    {
        let mut env = self.clone();
        let subst = env.fresh_substitution(b.kinds(), |kind, var_index| UniversalVar {
            kind,
            var_index,
        });
        (env, subst)
    }

    /// Given a bound value `<X..> v`, returns `v` with `X...` replaced with fresh universal variables.
    /// Modifies `self` to bring those variables into scope.
    pub fn instantiate_universally<T>(&self, b: &Binder<T>) -> (Env, T)
    where
        T: Fold,
    {
        let mut env = self.clone();
        let subst = env.fresh_substitution(b.kinds(), |kind, var_index| UniversalVar {
            kind,
            var_index,
        });
        let value = b.instantiate_with(&subst).unwrap();
        (env, value)
    }
    /// Returns `(e,s)` where
    ///
    /// * `s` is a list of fresh inference variables for each variable bound in `b`; and,
    /// * `e` is an extended environment with these inference variables in scope.
    pub fn existential_substitution<T>(&self, b: &Binder<T>) -> (Env, Vec<ExistentialVar>)
    where
        T: Fold,
    {
        let mut env = self.clone();
        let subst = env.fresh_substitution(b.kinds(), |kind, var_index| ExistentialVar {
            kind,
            var_index,
        });
        (env, subst)
    }

    /// Given a bound value `<X..> v`, returns `(env, v)` where `v` has `X...` replaced
    /// with fresh inference variables and `env` brings those variables into scope.
    pub fn instantiate_existentially<T>(&self, b: &Binder<T>) -> (Env, T)
    where
        T: Fold,
    {
        let (env, subst) = self.existential_substitution(b);
        let result = b.instantiate_with(&subst).unwrap();
        (env, result)
    }

    /// Given a set of variables that was returned by
    /// `existential_substitution` or `universal_substitution`,
    /// removes those variables from `self` along with anything created afterwards.
    /// Returns the list of variables created since the universal subst.
    pub(crate) fn pop_vars<V>(&mut self, v: &[V]) -> Set<Variable>
    where
        V: Upcast<Variable> + Copy,
    {
        if v.is_empty() {
            return set![];
        }

        let p0 = v[0];
        let universe_p0 = self.universe(p0);
        for (i, item) in v.iter().enumerate().skip(1) {
            assert_eq!(self.universe(item).index, universe_p0.index + i);
        }

        self.variables.drain(universe_p0.index..).collect()
    }

    /// Retain only those variables found in `vs`, returns the discarded variables
    pub fn remove_variables_unless_within(&mut self, vs: &[Variable]) -> Vec<Variable> {
        let (kept, discarded) = self.variables.iter().partition(|v| vs.contains(v));
        self.variables = kept;
        discarded
    }

    pub fn variables(&self) -> &[Variable] {
        &self.variables
    }

    // TODO: revisit this again, might need to update substitution.
    pub fn substitute(&self, vs: &VarSubstitution) -> Self {
        Self {
            variables: self
                .variables
                .iter()
                .map(|&v| vs.map_var(v).unwrap_or(v))
                .collect(),
            bias: self.bias,
            pending: vs.apply(&self.pending),
            allow_pending_outlives: self.allow_pending_outlives,
            known_true: self.known_true,
            substitution: self.substitution.clone(),
        }
    }

    pub fn defines(&self, v: Variable) -> bool {
        self.variables.contains(&v)
    }

    // TODO(step 1): implement this for the upcast below
    // Construct a set of constraints from a set of substitutions and the previous environment.
    //pub fn from(
    //    env: impl Upcast<Env>,
    //    iter: impl IntoIterator<Item = (impl Upcast<Variable>, impl Upcast<Parameter>)>,
    //) -> Self {
    //    let env = env.upcast();
    //    let substitution: Substitution = iter.into_iter().collect();
    //    assert!(env.encloses(substitution.range()));
    //    assert!(env.encloses(substitution.domain()));
    //    let c2 = Constraints {
    //        env,
    //        substitution,
    //        known_true: true,
    //    };
    //    c2.assert_valid();
    //    c2
    //}
}

impl CoreVisit<crate::prove::prove::FormalityLang> for Env {
    fn free_variables(&self) -> Vec<Variable> {
        self.variables.clone()
    }

    fn size(&self) -> usize {
        0
    }

    fn assert_valid(&self) {
        // no duplicates in `self.variables`
        let s: Set<Variable> = self.variables.iter().copied().collect();
        assert_eq!(s.len(), self.variables.len());
    }
}

impl UpcastFrom<()> for Env {
    fn upcast_from((): ()) -> Self {
        Env::default()
    }
}

// TODO(step 0): complete this, this is following what we previously do for prove_existential_var_eq
// maybe there is a better way

//impl<E, A, B> UpcastFrom<(E, (A, B))> for Env
//where
//    E: Upcast<Env>,
//    A: Upcast<Variable>,
//    B: Upcast<Parameter>,
//{
//    fn upcast_from((env, pair): (E, (A, B))) -> Self {
//        Env::from(env, std::iter::once(pair))
//    }
//}
