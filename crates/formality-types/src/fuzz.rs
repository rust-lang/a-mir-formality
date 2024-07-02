//! Fuzzing context for generating "well kinded" Rust types.
//!
//! See the Formality Book [chapter on fuzzing][f] for more details.
//!
//! [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html

use formality_core::{
    fuzz::{FuzzSingleton, SetGuard},
    Map,
};

use crate::grammar::{AdtId, AssociatedItemId, ParameterKind, TraitId};

/// The "Fuzz Context" stores the set of ADTs/traits/associated types and their associated kinds.
/// It can be "installed" using `FuzzCz::install` and then when we generate types we will reference it.
///
/// See the Formality Book [chapter on fuzzing][f] for more details.
///
/// [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html
pub struct FuzzCx {
    pub adt_kinds: Map<AdtId, Vec<ParameterKind>>,
    pub trait_kinds: Map<TraitId, Vec<ParameterKind>>,
    pub associated_items: Map<AssociatedItemId, (TraitId, Vec<ParameterKind>)>,
}

/// Map from each `AdtId` to its parameter kinds. Modified by `FuzzCx::install` and referenced in the other methods.
static ADT_ID_MAP: FuzzSingleton<Map<AdtId, Vec<ParameterKind>>> = FuzzSingleton::new();

/// Map from each `TraitId` to its parameter kinds. Modified by `FuzzCx::install` and referenced in the other methods.
static TRAIT_ID_MAP: FuzzSingleton<Map<TraitId, Vec<ParameterKind>>> = FuzzSingleton::new();

/// Map from each `AssociatedItemId` to its parameter kinds. Modified by `FuzzCx::install` and referenced in the other methods.
static ASSOCIATED_ID_MAP: FuzzSingleton<Map<AssociatedItemId, (TraitId, Vec<ParameterKind>)>> =
    FuzzSingleton::new();

/// Guard returned by `FuzzCx::install` to remove this context from scope.
pub struct FuzzCxGuard {
    guards: Vec<SetGuard<'static>>,
}

impl FuzzCx {
    /// Setup a fuzzing session for fuzzing Rust types.
    /// Returns a guard to uninstall the context.
    ///
    /// Trying to fuzz a type instance will panic if this method has not been
    /// called or if the returned guard has been dropped.
    ///
    /// The returned guard must be dropped before `FuzzCx::install` can be used again.
    pub fn install(self) -> FuzzCxGuard {
        let mut guards: Vec<SetGuard<'static>> = vec![];

        let adt_ids: Vec<AdtId> = self.adt_kinds.iter().map(|(k, _v)| k).cloned().collect();
        guards.push(AdtId::fuzz_pool().set(adt_ids));

        let trait_ids: Vec<TraitId> = self.trait_kinds.iter().map(|(k, _v)| k).cloned().collect();
        guards.push(TraitId::fuzz_pool().set(trait_ids));

        guards.push(ADT_ID_MAP.set(self.adt_kinds));

        guards.push(TRAIT_ID_MAP.set(self.trait_kinds));

        FuzzCxGuard { guards }
    }

    /// Access the installed map from adt-id to associated parameter kinds.
    pub fn adt_id_map() -> &'static FuzzSingleton<Map<AdtId, Vec<ParameterKind>>> {
        &ADT_ID_MAP
    }

    /// Access the installed map from trait-id to associated parameter kinds.
    pub fn trait_id_map() -> &'static FuzzSingleton<Map<TraitId, Vec<ParameterKind>>> {
        &TRAIT_ID_MAP
    }

    /// Access the installed map from associated item id to associated parameter kinds.
    pub fn associated_id_map(
    ) -> &'static FuzzSingleton<Map<AssociatedItemId, (TraitId, Vec<ParameterKind>)>> {
        &ASSOCIATED_ID_MAP
    }
}
