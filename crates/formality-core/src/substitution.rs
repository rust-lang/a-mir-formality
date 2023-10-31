use crate::{
    cast::{Upcast, UpcastFrom},
    collections::{Map, Set},
    fold::Fold,
    language::{Language, Parameter},
    variable::Variable,
    visit::Visit,
};

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Substitution<L: Language> {
    map: Map<Variable<L>, Parameter<L>>,
}

impl<L: Language> Substitution<L> {
    /// Returns the variables that will be substituted for a new value.
    pub fn domain(&self) -> Set<Variable<L>> {
        self.map.keys().cloned().collect()
    }

    /// Returns the parameters that that will be substituted for a new value.
    pub fn range(&self) -> Set<Parameter<L>> {
        self.map.values().cloned().collect()
    }

    /// True if `v` is in this substitution's domain
    pub fn maps(&self, v: Variable<L>) -> bool {
        self.map.contains_key(&v)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Variable<L>, Parameter<L>)> + '_ {
        self.map.iter().map(|(v, p)| (*v, p.clone()))
    }

    /// An empty substitution is just the identity function.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<L: Language> std::fmt::Debug for Substitution<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_set();
        for (v, p) in self.iter() {
            f.entry(&Entry { v, p });
            struct Entry<L: Language> {
                v: Variable<L>,
                p: Parameter<L>,
            }
            impl<L: Language> std::fmt::Debug for Entry<L> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{:?} => {:?}", self.v, self.p)
                }
            }
        }
        f.finish()
    }
}

impl<L: Language, Vs> std::ops::SubAssign<Vs> for Substitution<L>
where
    Vs: Upcast<Vec<Variable<L>>>,
{
    fn sub_assign(&mut self, rhs: Vs) {
        let rhs: Vec<Variable<L>> = rhs.upcast();

        for v in rhs {
            self.map.remove(&v);
        }
    }
}

impl<L: Language> IntoIterator for Substitution<L> {
    type Item = (Variable<L>, Parameter<L>);

    type IntoIter = std::collections::btree_map::IntoIter<Variable<L>, Parameter<L>>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<L: Language, A, B> Extend<(A, B)> for Substitution<L>
where
    A: Upcast<Variable<L>>,
    B: Upcast<Parameter<L>>,
{
    fn extend<T: IntoIterator<Item = (A, B)>>(&mut self, iter: T) {
        self.map
            .extend(iter.into_iter().map(|(v, p)| (v.upcast(), p.upcast())));
    }
}

impl<L: Language, A, B> FromIterator<(A, B)> for Substitution<L>
where
    A: Upcast<Variable<L>>,
    B: Upcast<Parameter<L>>,
{
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        let mut s = Substitution::default();
        s.extend(iter);
        s
    }
}

impl<L: Language, A, B> UpcastFrom<(A, B)> for Substitution<L>
where
    A: Upcast<Variable<L>>,
    B: Upcast<Parameter<L>>,
{
    fn upcast_from(term: (A, B)) -> Self {
        let term: (Variable<L>, Parameter<L>) = term.upcast();
        std::iter::once(term).collect()
    }
}

impl<L: Language> Substitution<L> {
    pub fn apply<T: Fold<L>>(&self, t: &T) -> T {
        t.substitute(&mut |v| self.map.get(&v).cloned())
    }

    pub fn get(&self, v: Variable<L>) -> Option<Parameter<L>> {
        self.map.get(&v).cloned()
    }
}

impl<L: Language> Fold<L> for Substitution<L> {
    fn substitute(&self, substitution_fn: crate::fold::SubstitutionFn<'_, L>) -> Self {
        self.iter()
            .map(|(v, p)| (v, p.substitute(substitution_fn)))
            .collect()
    }
}

impl<L: Language> Visit<L> for Substitution<L> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        let mut v = self.range().free_variables();
        v.extend(self.domain());
        v
    }

    fn size(&self) -> usize {
        self.range().iter().map(|r| r.size()).sum()
    }

    fn assert_valid(&self) {
        self.range().assert_valid()
    }
}

impl<L: Language> std::ops::Index<Variable<L>> for Substitution<L> {
    type Output = Parameter<L>;

    fn index(&self, index: Variable<L>) -> &Self::Output {
        &self.map[&index]
    }
}

/// A substitution that is only between variables.
/// These are reversible.
#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarSubstitution<L: Language> {
    map: Map<Variable<L>, Variable<L>>,
}

impl<L, A, B> Extend<(A, B)> for VarSubstitution<L>
where
    L: Language,
    A: Upcast<Variable<L>>,
    B: Upcast<Variable<L>>,
{
    fn extend<T: IntoIterator<Item = (A, B)>>(&mut self, iter: T) {
        self.map
            .extend(iter.into_iter().map(|(v, p)| (v.upcast(), p.upcast())));
    }
}

impl<L, A, B> FromIterator<(A, B)> for VarSubstitution<L>
where
    L: Language,
    A: Upcast<Variable<L>>,
    B: Upcast<Variable<L>>,
{
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        let mut s = VarSubstitution::default();
        s.extend(iter);
        s
    }
}

impl<L: Language> VarSubstitution<L> {
    pub fn reverse(&self) -> VarSubstitution<L> {
        self.map.iter().map(|(k, v)| (*v, *k)).collect()
    }

    pub fn apply<T: Fold<L>>(&self, t: &T) -> T {
        t.substitute(&mut |v| Some(self.map.get(&v)?.upcast()))
    }

    pub fn map_var(&self, v: Variable<L>) -> Option<Variable<L>> {
        self.map.get(&v).copied()
    }

    pub fn maps_var(&self, v: Variable<L>) -> bool {
        self.map.contains_key(&v)
    }

    pub fn insert_mapping(&mut self, from: impl Upcast<Variable<L>>, to: impl Upcast<Variable<L>>) {
        let x = self.map.insert(from.upcast(), to.upcast());
        assert!(x.is_none());
    }
}
