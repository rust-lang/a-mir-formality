use formality_macros::term;
use formality_types::{
    cast::{Downcasted, Upcast},
    grammar::{
        AdtId, AssociatedItemId, Binder, CrateId, Fallible, FieldId, FnId, Predicate, TraitId,
        TraitRef, Ty,
    },
};

#[term($crates)]
pub struct Program {
    /// List of all crates.
    /// The last crate in the list is the current crate.
    pub crates: Vec<Crate>,
}

impl Program {
    /// Id of the current crate.
    pub fn current_crate_id(&self) -> CrateId {
        self.crates.last().unwrap().id.clone()
    }

    /// Iterator over items from all crates.
    pub fn items_from_all_crates(&self) -> impl Iterator<Item = &CrateItem> {
        self.crates.iter().flat_map(|c| &c.items)
    }

    /// Find the ADT with the given name.
    pub fn adt_named(&self, adt_id: &AdtId) -> Fallible<Adt> {
        self.items_from_all_crates()
            .downcasted::<Adt>()
            .find(|adt| adt.id == *adt_id)
            .ok_or_else(|| anyhow::format_err!("no adt named `{adt_id:?}`"))
    }

    /// Find the trait with the given name.
    pub fn trait_named(&self, trait_id: &TraitId) -> Fallible<Trait> {
        self.items_from_all_crates()
            .downcasted::<Trait>()
            .find(|t| t.id == *trait_id)
            .ok_or_else(|| anyhow::format_err!("no trait named `{trait_id:?}`"))
    }

    /// Find the trait with the given name.
    pub fn fn_named(&self, fn_id: &FnId) -> Fallible<Fn> {
        self.items_from_all_crates()
            .downcasted::<Fn>()
            .find(|t| t.id == *fn_id)
            .ok_or_else(|| anyhow::format_err!("no trait named `{fn_id:?}`"))
    }
}

#[term(crate $id { $*items })]
pub struct Crate {
    pub id: CrateId,
    pub items: Vec<CrateItem>,
}

#[term]
pub enum CrateItem {
    #[cast]
    Adt(Adt),
    #[cast]
    Trait(Trait),
    #[cast]
    TraitImpl(TraitImpl),
    #[cast]
    Fn(Fn),
}

#[term($kind $id $binder)]
pub struct Adt {
    pub kind: AdtKind,
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

#[term]
pub enum AdtKind {
    Struct,
    Enum,
    Union,
}

#[term(where $where_clauses { $*variants })]
pub struct AdtBoundData {
    pub where_clauses: Vec<Predicate>,
    pub variants: Vec<AdtVariant>,
}

impl AdtBoundData {
    pub fn variant_named(&self, variant_id: &VariantId) -> Fallible<&AdtVariant> {
        self.variants
            .iter()
            .find(|v| v.name == *variant_id)
            .ok_or_else(|| anyhow::format_err!("no ADT variant named `{variant_id:?}`"))
    }
}

formality_types::id!(VariantId);

impl VariantId {
    /// Returns the special variant-id used for the single variant of a struct.
    pub fn for_struct() -> Self {
        VariantId::new("struct")
    }
}

#[term($name { $*fields })]
pub struct AdtVariant {
    pub name: VariantId,
    pub fields: Vec<Field>,
}

impl AdtVariant {
    pub fn field_named(&self, name: impl Upcast<FieldName>) -> Fallible<&Field> {
        let name = name.upcast();
        self.fields
            .iter()
            .find(|f| f.name == name)
            .ok_or_else(|| anyhow::format_err!("no field named `{name:?}`"))
    }
}

#[term($name : $ty)]
pub struct Field {
    pub name: FieldName,
    pub ty: Ty,
}

#[term]
pub enum FieldName {
    #[cast]
    Id(FieldId),
    #[cast]
    Index(usize),
}

#[term(trait $id $binder)]
pub struct Trait {
    pub id: TraitId,
    pub binder: Binder<TraitBoundData>,
}

#[term(where $where_clauses { $*trait_items })]
pub struct TraitBoundData {
    pub where_clauses: Vec<Predicate>,
    pub trait_items: Vec<TraitItem>,
}

#[term]
pub enum TraitItem {
    #[cast]
    Fn(Fn),
    #[cast]
    AssociatedTy(AssociatedTy),
}

#[term(fn $id $binder)]
pub struct Fn {
    pub id: FnId,
    pub binder: Binder<FnBoundData>,
}

#[term($input_tys -> $output_ty where $where_clauses)]
pub struct FnBoundData {
    pub input_tys: Vec<Ty>,
    pub output_ty: Ty,
    pub where_clauses: Vec<Predicate>,
}

#[term(type $id $binder)]
pub struct AssociatedTy {
    pub id: AssociatedItemId,
    pub binder: Binder<AssociatedTyBoundData>,
}

#[term(: $ensures where $where_clauses)]
pub struct AssociatedTyBoundData {
    /// The Binder binds a single variable that is the name for
    /// associated type. So e.g. `type Item: Sized` would be encoded
    /// as `<type I> (I: Sized)`.
    pub ensures: Binder<Vec<Predicate>>,

    /// Where clauses that must hold.
    pub where_clauses: Vec<Predicate>,
}

#[term(impl $binder)]
pub struct TraitImpl {
    pub binder: Binder<TraitImplBoundData>,
}

#[term($trait_ref where $where_clauses { $*impl_items })]
pub struct TraitImplBoundData {
    pub trait_ref: TraitRef,
    pub where_clauses: Vec<Predicate>,
    pub impl_items: Vec<ImplItem>,
}

#[term]
pub enum ImplItem {
    #[cast]
    Fn(Fn),
    #[cast]
    AssociatedTyValue(AssociatedTyValue),
}

#[term(type $id $binder)]
pub struct AssociatedTyValue {
    pub id: AssociatedItemId,
    pub binder: Binder<AssociatedTyValueBoundData>,
}

#[term(where $where_clauses = $ty)]
pub struct AssociatedTyValueBoundData {
    pub where_clauses: Vec<Predicate>,
    pub ty: Ty,
}
