mod subtype;

pub struct Universe {
    pub index: usize,
}

#[derive(Interned)]
pub struct Ty {
    data: &'static TyData,
}

pub enum TyData {
    RigidTy(RigidTy),
    AliasTy(AliasTy),
    PredicateTy(PredicateTy),
    PlaceholderTy(PlaceholderTy),
    InferenceVar(VarId),
    BoundVar(VarId),
}

pub struct RigidTy {
    name: RigidName,
    parameters: Parameters,
}

pub enum RigidName {
    Adt(AdtId),
    ScalarId(ScalarId),
    Ref(Mut),
    Tuple(usize),
    FnPtr(usize),
    FnDef(FnId),
}

pub struct AliasTy {
    name: AliasName,
    parameters: Parameters,
}

pub enum AliasName {
    AssocTyId(AssocItemId),
}

pub struct AssocItemId {
    trait_id: TraitId,
    item_id: ItemId,
}

pub enum PredicateTy {
    QuantifiedTy(Binder<Ty>),
}

pub struct PlaceholderTy {
    universe: Universe,
    parameters: Parameters,
}

pub enum QuantifierKind {
    ForAll,
    Exists,
}

pub struct Binder<T> {
    pub kind: QuantifierKind,
    pub variables: Vec<KindedVarId>,
    pub data: T,
}

pub struct KindedVarId {
    kind: ParameterKind,
    id: VarId,
}

pub enum Parameter {
    Ty(Ty),
    Lt(Lt),
}

pub enum ParameterKind {
    Ty,
    Lt,
}

pub struct VarId(String);
