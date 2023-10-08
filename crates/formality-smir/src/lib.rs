#![feature(rustc_private)]

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
