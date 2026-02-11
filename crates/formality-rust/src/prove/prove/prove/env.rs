use crate::grammar::{
    Binder, ExistentialVar, ParameterKind, UniversalVar, VarIndex, VarSubstitution, Variable, Wc,
};
use crate::rust::{Fold, Visit};
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
    variables: Vec<Variable>,

    /// XXX this needs to be explained
    bias: Bias,

    /// Pending goals that have not yet been proven.
    /// When we prove a conjunction `(A && B)`, proving `A` may result in
    /// some pending items (e.g., subtype constraints).
    ///
    /// These are then added into the env when we prove `B` so that if `B`
    /// winds up constraining an inference varibale, it can also try to prove
    /// the pending constraints that are now unlocked.
    ///
    /// Whenever a "successful" proof results, the pending obligations
    pending: Vec<Wc>,

    /// When true, outlives constraints like `'a: 'b` can be deferred as pending
    /// obligations rather than being proven immediately. This is used during
    /// type checking to accumulate constraints that will be verified by the
    /// borrow checker. When false, outlives must be proven from assumptions.
    allow_pending_outlives: bool,
}

impl Env {
    pub fn new_with_bias(bias: Bias) -> Self {
        Env {
            variables: Default::default(),
            bias,
            pending: vec![],
            allow_pending_outlives: false,
        }
    }

    pub fn only_universal_variables(&self) -> bool {
        self.variables.iter().all(|v| v.is_universal())
    }

    pub fn bias(&self) -> Bias {
        self.bias
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
}

cast_impl!(Env);

#[term(U($index))]
#[derive(Copy)]
pub struct Universe {
    index: usize,
}

impl Env {
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
    pub fn instantiate_universally<T>(&mut self, b: &Binder<T>) -> T
    where
        T: Fold,
    {
        let subst = self.fresh_substitution(b.kinds(), |kind, var_index| UniversalVar {
            kind,
            var_index,
        });
        b.instantiate_with(&subst).unwrap()
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

    /// Given a bound value `<X..> v`, returns `v` with `X...` replaced with fresh inference variables.
    /// Modifies `self` to bring those variables into scope.
    pub fn instantiate_existentially<T>(&mut self, b: &Binder<T>) -> T
    where
        T: Fold,
    {
        let (env, subst) = self.existential_substitution(b);
        let result = b.instantiate_with(&subst).unwrap();
        *self = env;
        result
    }

    /// Given a set of variables that was returned by
    /// `existential_substitution` or `universal_substitution`,
    /// removes those variables from `self` along with anything created afterwards.
    /// Returns the list of variables created since the universal subst.
    pub(crate) fn pop_vars<V>(&mut self, v: &[V]) -> Vec<Variable>
    where
        V: Upcast<Variable> + Copy,
    {
        if v.is_empty() {
            return vec![];
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
        }
    }

    pub fn defines(&self, v: Variable) -> bool {
        self.variables.contains(&v)
    }
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
