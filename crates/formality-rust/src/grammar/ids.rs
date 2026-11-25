use formality_core::id;

id!(ValueId);
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

impl TraitId {
    const SIZED: &'static str = "Sized";

    /// Returns the id of the `Sized` trait.
    pub fn sized() -> Self {
        TraitId::new(Self::SIZED)
    }

    /// True if this is the `Sized` trait.
    pub fn is_sized(&self) -> bool {
        self.as_str() == Self::SIZED
    }
}
