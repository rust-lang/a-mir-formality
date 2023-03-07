use formality_macros::term;
use formality_types::{
    cast::{To, Upcast},
    cast_impl,
    collections::Set,
    fold::Fold,
    grammar::{Binder, InferenceVar, ParameterKind, PlaceholderVar, VarIndex, Variable},
    visit::Visit,
};

#[derive(Default, Debug, Clone, Hash, Ord, Eq, PartialEq, PartialOrd)]
pub struct Env {
    variables: Vec<Variable>,
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
                Variable::PlaceholderVar(pv) => pv.var_index.index,
                Variable::InferenceVar(iv) => iv.var_index.index,
                Variable::BoundVar(_) => panic!(),
            })
            .max()
            .unwrap_or(0);

        VarIndex {
            index: max_index + 1,
        }
    }

    pub fn insert_fresh_before(&mut self, kind: ParameterKind, rank: Universe) -> InferenceVar {
        let var_index = self.fresh_index();
        let v = InferenceVar { kind, var_index };
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

    /// An env A *encloses* a term `v` if all free variables in `v`
    /// are defined in A.
    pub fn assert_encloses(&self, v: impl Visit) {
        v.free_variables().into_iter().for_each(|v| {
            assert!(self.variables.contains(&v));
        });
    }

    /// An env A is a *valid extension* of an env B if
    /// it contains a superset of the variables in B and
    /// it respects the ordering of all variables in B.
    pub fn assert_valid_extension_of(&self, env0: &Env) {
        self.assert_encloses(env0);
        for i in 1..env0.variables.len() {
            let v0_a = env0.variables[i - 1];
            let v0_b = env0.variables[i];
            assert!(self.universe(v0_a) < self.universe(v0_b));
        }
    }

    fn fresh_substituion<V>(
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

    pub fn universal_substitution<T>(&self, b: &Binder<T>) -> (Env, Vec<PlaceholderVar>)
    where
        T: Fold,
    {
        let mut env = self.clone();
        let subst = env.fresh_substituion(b.kinds(), |kind, var_index| PlaceholderVar {
            kind,
            var_index,
        });
        (env, subst)
    }

    pub fn existential_substitution<T>(&self, b: &Binder<T>) -> (Env, Vec<InferenceVar>)
    where
        T: Fold,
    {
        let mut env = self.clone();
        let subst = env.fresh_substituion(b.kinds(), |kind, var_index| InferenceVar {
            kind,
            var_index,
        });
        (env, subst)
    }

    /// Given a set of variables that was returned by
    /// `existential_substitution` or `universal_substitution`,
    /// removes those variables from `self` along with anything created afterwards.
    /// Returns the list of variables created since the universal subst.
    pub(crate) fn pop_vars<V>(&mut self, v: &[V]) -> Vec<Variable>
    where
        V: Upcast<Variable> + Copy,
    {
        if v.len() == 0 {
            return vec![];
        }

        let p0 = v[0];
        let universe_p0 = self.universe(p0);
        for i in 1..v.len() {
            assert_eq!(self.universe(v[i]).index, universe_p0.index + i);
        }

        self.variables.drain(universe_p0.index..).collect()
    }
}

impl Visit for Env {
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
