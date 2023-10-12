#![feature(rustc_private)]

/// This import is needed, because `stable_mir` on its own doesn't have the `scoped_tls` rlib.
extern crate rustc_driver;
/// Access to the pre-0.1 stable_mir crate
extern crate stable_mir;

/// Trait used to convert from Stable MIR to Formality types.
pub trait ToFormality {
    /// The formality representation of the stable MIR type implementing ToFormality.
    type T;
    /// Converts an object to the equivalent Formality representation.
    fn formality(&self) -> Self::T;
}

impl ToFormality for stable_mir::ty::GenericParamDefKind {
    type T = formality_types::derive_links::ParameterKind;

    fn formality(&self) -> Self::T {
        use formality_types::derive_links::ParameterKind;

        match self {
            stable_mir::ty::GenericParamDefKind::Lifetime => ParameterKind::Lt,
            stable_mir::ty::GenericParamDefKind::Type { .. } => ParameterKind::Ty,
            stable_mir::ty::GenericParamDefKind::Const { .. } => ParameterKind::Const,
        }
    }
}
