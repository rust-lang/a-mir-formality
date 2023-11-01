use crate::{
    cast::{Upcast, UpcastFrom},
    collections::{Map, Set},
    fold::CoreFold,
    language::{CoreParameter, Language},
    variable::CoreVariable,
    visit::CoreVisit,
};

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CoreSubstitution<L: Language> {
    map: Map<CoreVariable<L>, CoreParameter<L>>,
}

impl<L: Language> CoreSubstitution<L> {
    /// Returns the variables that will be substituted for a new value.
    pub fn domain(&self) -> Set<CoreVariable<L>> {
        self.map.keys().cloned().collect()
    }

    /// Returns the parameters that that will be substituted for a new value.
    pub fn range(&self) -> Set<CoreParameter<L>> {
        self.map.values().cloned().collect()
    }

    /// True if `v` is in this substitution's domain
    pub fn maps(&self, v: CoreVariable<L>) -> bool {
        self.map.contains_key(&v)
    }

    pub fn iter(&self) -> impl Iterator<Item = (CoreVariable<L>, CoreParameter<L>)> + '_ {
        self.map.iter().map(|(v, p)| (*v, p.clone()))
    }

    /// An empty substitution is just the identity function.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<L: Language> std::fmt::Debug for CoreSubstitution<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_set();
        for (v, p) in self.iter() {
            f.entry(&Entry { v, p });
            struct Entry<L: Language> {
                v: CoreVariable<L>,
                p: CoreParameter<L>,
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

impl<L: Language, Vs> std::ops::SubAssign<Vs> for CoreSubstitution<L>
where
    Vs: Upcast<Vec<CoreVariable<L>>>,
{
    fn sub_assign(&mut self, rhs: Vs) {
        let rhs: Vec<CoreVariable<L>> = rhs.upcast();

        for v in rhs {
            self.map.remove(&v);
        }
    }
}

impl<L: Language> IntoIterator for CoreSubstitution<L> {
    type Item = (CoreVariable<L>, CoreParameter<L>);

    type IntoIter = std::collections::btree_map::IntoIter<CoreVariable<L>, CoreParameter<L>>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<L: Language, A, B> Extend<(A, B)> for CoreSubstitution<L>
where
    A: Upcast<CoreVariable<L>>,
    B: Upcast<CoreParameter<L>>,
{
    fn extend<T: IntoIterator<Item = (A, B)>>(&mut self, iter: T) {
        self.map
            .extend(iter.into_iter().map(|(v, p)| (v.upcast(), p.upcast())));
    }
}

impl<L: Language, A, B> FromIterator<(A, B)> for CoreSubstitution<L>
where
    A: Upcast<CoreVariable<L>>,
    B: Upcast<CoreParameter<L>>,
{
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        let mut s = CoreSubstitution::default();
        s.extend(iter);
        s
    }
}

impl<L: Language, A, B> UpcastFrom<(A, B)> for CoreSubstitution<L>
where
    A: Upcast<CoreVariable<L>>,
    B: Upcast<CoreParameter<L>>,
{
    fn upcast_from(term: (A, B)) -> Self {
        let term: (CoreVariable<L>, CoreParameter<L>) = term.upcast();
        std::iter::once(term).collect()
    }
}

impl<L: Language> CoreSubstitution<L> {
    pub fn apply<T: CoreFold<L>>(&self, t: &T) -> T {
        t.substitute(&mut |v| self.map.get(&v).cloned())
    }

    pub fn get(&self, v: CoreVariable<L>) -> Option<CoreParameter<L>> {
        self.map.get(&v).cloned()
    }
}

impl<L: Language> CoreFold<L> for CoreSubstitution<L> {
    fn substitute(&self, substitution_fn: crate::fold::SubstitutionFn<'_, L>) -> Self {
        self.iter()
            .map(|(v, p)| (v, p.substitute(substitution_fn)))
            .collect()
    }
}

impl<L: Language> CoreVisit<L> for CoreSubstitution<L> {
    fn free_variables(&self) -> Vec<CoreVariable<L>> {
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

impl<L: Language> std::ops::Index<CoreVariable<L>> for CoreSubstitution<L> {
    type Output = CoreParameter<L>;

    fn index(&self, index: CoreVariable<L>) -> &Self::Output {
        &self.map[&index]
    }
}

/// A substitution that is only between variables.
/// These are reversible.
#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CoreVarSubstitution<L: Language> {
    map: Map<CoreVariable<L>, CoreVariable<L>>,
}

impl<L, A, B> Extend<(A, B)> for CoreVarSubstitution<L>
where
    L: Language,
    A: Upcast<CoreVariable<L>>,
    B: Upcast<CoreVariable<L>>,
{
    fn extend<T: IntoIterator<Item = (A, B)>>(&mut self, iter: T) {
        self.map
            .extend(iter.into_iter().map(|(v, p)| (v.upcast(), p.upcast())));
    }
}

impl<L, A, B> FromIterator<(A, B)> for CoreVarSubstitution<L>
where
    L: Language,
    A: Upcast<CoreVariable<L>>,
    B: Upcast<CoreVariable<L>>,
{
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        let mut s = CoreVarSubstitution::default();
        s.extend(iter);
        s
    }
}

impl<L: Language> CoreVarSubstitution<L> {
    pub fn reverse(&self) -> CoreVarSubstitution<L> {
        self.map.iter().map(|(k, v)| (*v, *k)).collect()
    }

    pub fn apply<T: CoreFold<L>>(&self, t: &T) -> T {
        t.substitute(&mut |v| Some(self.map.get(&v)?.upcast()))
    }

    pub fn map_var(&self, v: CoreVariable<L>) -> Option<CoreVariable<L>> {
        self.map.get(&v).copied()
    }

    pub fn maps_var(&self, v: CoreVariable<L>) -> bool {
        self.map.contains_key(&v)
    }

    pub fn insert_mapping(
        &mut self,
        from: impl Upcast<CoreVariable<L>>,
        to: impl Upcast<CoreVariable<L>>,
    ) {
        let x = self.map.insert(from.upcast(), to.upcast());
        assert!(x.is_none());
    }
}
