use crate::{
    fold::CoreFold,
    fuzz::{FuzzCx, Fuzzable},
    language::Language,
};

use super::CoreBinder;

/// A vector of variable kinds created by fuzzing.
/// The length of the vector is controlled by the configured binder range.
#[derive(Debug)]
pub struct KindVec<L: Language> {
    vec: Vec<L::Kind>,
}

impl<L: Language> KindVec<L> {
    pub fn into_vec(self) -> Vec<L::Kind> {
        self.vec
    }
}

impl<L: Language> std::ops::Deref for KindVec<L> {
    type Target = [L::Kind];

    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl<L: Language> Fuzzable<L> for KindVec<L>
where
    L::Kind: Fuzzable<L>,
{
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
            guard.binder_range().len() as f64 * guard.estimate_cardinality::<L::Kind>()
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let binder_range = guard.binder_range().clone();
            let len = guard.fuzz_usize(binder_range)?;
            let vec = (0..len)
                .map(|_| guard.fuzz::<L::Kind>())
                .collect::<Option<_>>()?;
            Some(KindVec { vec })
        })
    }
}

impl<L: Language, T> Fuzzable<L> for CoreBinder<L, T>
where
    T: Fuzzable<L> + CoreFold<L>,
{
    fn is_binder() -> bool {
        true
    }

    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
            guard.estimate_cardinality::<KindVec<L>>() * guard.estimate_cardinality::<T>()
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let kinds: KindVec<L> = guard.fuzz()?;
            guard.introduce_free_variables_of_kinds(&kinds, |guard, variables| {
                let bound_term = guard.fuzz::<T>()?;
                Some(CoreBinder::new(variables, bound_term))
            })
        })
    }
}
