use formality_core::id;

id!(FnId);
id!(AdtId);
id!(TraitId);
id!(AssociatedItemId);
id!(CrateId);
id!(FieldId);
id!(VariantId);

impl VariantId {
    /// Returns the special variant-id used for the single variant of a struct.
    pub fn for_struct() -> Self {
        VariantId::new("struct")
    }
}
