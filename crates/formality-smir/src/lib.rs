/// Trait used to convert from Stable MIR to Formality types.
pub trait ToFormality {
    /// The formality representation of the stable MIR type implementing ToFormality.
    type T;
    /// Converts an object to the equivalent Formality representation.
    fn formality(&self) -> Self::T;
}
