use crate::{fold::CoreFold, fuzz::PushGuard, language::Language, variable::CoreBoundVar};

use super::{fresh_bound_var, CoreBinder};

/// Brings new variables into scope for fuzzing.
/// Don't invoke directly, instead call `L::open_fuzz_binder`.
pub(crate) fn open_fuzz_binder_impl<L>(kinds: &[L::Kind]) -> PushKindGuard<L>
where
    L: Language,
{
    let variables: Vec<_> = kinds.iter().map(|k| fresh_bound_var(*k)).collect();
    PushKindGuard {
        guard: L::fuzz_free_variables().fuzz_push(variables.clone()),
        variables,
    }
}

/// The guard returned when you open a binder for fuzzing.
/// It references a set of variables `V` that were brought into scope.
/// You should invoke `self.into_binder(t)` on the fuzzed term `t` to create a `Binder<T>`
/// where each variable in `V` is converted to a reference to the binder.
///
/// See the Formality Book [chapter on fuzzing][f] for more details.
///
/// [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html
#[must_use]
pub struct PushKindGuard<L: Language> {
    #[allow(dead_code)] // the point of this field is to run the destructor
    guard: PushGuard<'static, CoreBoundVar<L>>,
    variables: Vec<CoreBoundVar<L>>,
}

impl<L: Language> PushKindGuard<L> {
    /// Access the variables that were brought into scope.
    pub fn variables(&self) -> &Vec<CoreBoundVar<L>> {
        &self.variables
    }

    /// Convert into a binder.
    pub fn into_binder<T>(self, bound_term: T) -> CoreBinder<L, T>
    where
        T: CoreFold<L>,
    {
        CoreBinder::new(self.variables, bound_term)
    }
}

impl<L: Language, T> bolero::TypeGenerator for CoreBinder<L, T>
where
    T: bolero::TypeGenerator + CoreFold<L>,
    L::Kind: bolero::TypeGenerator,
{
    /// Generate a binder with some fresh data inside.
    fn generate<D: bolero::Driver>(driver: &mut D) -> Option<Self> {
        let kinds: Vec<L::Kind> = driver.gen()?;
        let guard = L::open_fuzz_binder(&kinds);
        let bound_term: T = driver.gen()?;
        Some(guard.into_binder(bound_term))
    }
}
