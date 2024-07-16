//! Fuzzing context for generating "well kinded" Rust types.
//!
//! See the Formality Book [chapter on fuzzing][f] for more details.
//!
//! [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html

use formality_core::{fuzz::FuzzConfig, Map};

use crate::{
    grammar::{AdtId, AssociatedItemId, ParameterKind, TraitId},
    rust::FormalityLang,
};

// ANCHOR: RustTypesFuzzConfigTrait
/// Methods for installing/querying the context needed to fuzz Rust types.
pub trait RustTypesFuzzConfig {
    /// Install the context needed to fuzz Rust types.
    ///
    /// * `adt_kinds`: the set of ADT names and their affiliated kinds.
    /// * `trait_kinds`: the set of trait names and their affiliated kinds.
    /// * `associated_items`: the set of associated items and their trait / affiliated kinds
    ///   (these are the GAT parameters and do not include those of the trait).
    fn with_rust_types(
        self,
        adt_kinds: Map<AdtId, Vec<ParameterKind>>,
        trait_kinds: Map<TraitId, Vec<ParameterKind>>,
        associated_items: Map<AssociatedItemId, (TraitId, Vec<ParameterKind>)>,
    ) -> Self;

    /// Fetch the kinds of `adt_id`
    fn adt_kinds(&self, adt_id: &AdtId) -> Vec<ParameterKind>;

    /// Fetch the kinds of `trait_id`
    fn trait_kinds(&self, trait_id: &TraitId) -> Vec<ParameterKind>;

    /// Fetch the trait / kinds of `associated_item_id`
    fn associated_item_kinds(
        &self,
        associated_item_id: &AssociatedItemId,
    ) -> (TraitId, Vec<ParameterKind>);
}
// ANCHOR_END: RustTypesFuzzConfigTrait

impl RustTypesFuzzConfig for FuzzConfig<FormalityLang> {
    // ANCHOR: RustTypesFuzzConfigImpl::with_rust_types
    fn with_rust_types(
        self,
        adt_kinds: Map<AdtId, Vec<ParameterKind>>,
        trait_kinds: Map<TraitId, Vec<ParameterKind>>,
        associated_items: Map<AssociatedItemId, (TraitId, Vec<ParameterKind>)>,
    ) -> Self {
        self
            // Set the valid values for AdtIds, TraitIds, and AssociatedItemId:
            .with_values(adt_kinds.iter().map(|(k, _v)| k).cloned().collect())
            .with_values(trait_kinds.iter().map(|(k, _v)| k).cloned().collect())
            .with_values(associated_items.iter().map(|(k, _v)| k).cloned().collect())
            // Set the maps that are fetchable below
            .with_key_values(adt_kinds)
            .with_key_values(trait_kinds)
            .with_key_values(associated_items)
    }
    // ANCHOR_END: RustTypesFuzzConfigImpl::with_rust_types

    // ANCHOR: RustTypesFuzzConfigImpl::adt_kinds
    fn adt_kinds(&self, adt_id: &AdtId) -> Vec<ParameterKind> {
        self.key_value(adt_id).unwrap()
    }
    // ANCHOR_END: RustTypesFuzzConfigImpl::adt_kinds

    fn trait_kinds(&self, trait_id: &TraitId) -> Vec<ParameterKind> {
        self.key_value(trait_id).unwrap()
    }

    fn associated_item_kinds(
        &self,
        associated_item_id: &AssociatedItemId,
    ) -> (TraitId, Vec<ParameterKind>) {
        self.key_value(associated_item_id).unwrap()
    }
}
