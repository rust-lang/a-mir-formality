use formality_core::{cast_impl, term};
use std::sync::Arc;

mod debug_impls;
mod parse_impls;
mod term_impls;
use formality_core::{DowncastTo, To, Upcast, UpcastFrom};

use super::{
    consts::Const, AdtId, AssociatedItemId, Binder, BoundVar, ExistentialVar, TraitId,
    UniversalVar, ValueId, Variable,
};

#[term]
pub enum Ty {
    #[cast]
    RigidTy(RigidTy),
    #[cast]
    AliasTy(AliasTy),
    #[cast]
    PredicateTy(PredicateTy),
    #[variable(ParameterKind::Ty)]
    Variable(Variable),
}

/// Temporary alias for migration -- allows `TyData::Variant` to still compile.
pub type TyData = Ty;

impl Ty {
    pub fn never() -> Self {
        RigidTy::new(RigidName::Never, ()).upcast()
    }

    pub fn to_parameter(&self) -> Parameter {
        self.clone().upcast()
    }

    pub fn rigid(name: impl Upcast<RigidName>, parameters: impl Upcast<Vec<Parameter>>) -> Self {
        RigidTy {
            name: name.upcast(),
            parameters: parameters.upcast(),
        }
        .upcast()
    }

    pub fn alias(name: impl Upcast<AliasName>, parameters: impl Upcast<Vec<Parameter>>) -> Self {
        AliasTy {
            name: name.upcast(),
            parameters: parameters.upcast(),
        }
        .upcast()
    }

    pub fn ref_ty(&self, l: impl Upcast<Lt>) -> Self {
        let l: Lt = l.upcast();
        Self::rigid(
            RefKind::Shared,
            vec![l.to::<Parameter>(), self.to::<Parameter>()],
        )
    }

    pub fn ref_mut_ty(&self, l: impl Upcast<Lt>) -> Self {
        let l: Lt = l.upcast();
        Self::rigid(
            RefKind::Mut,
            vec![l.to::<Parameter>(), self.to::<Parameter>()],
        )
    }

    pub fn ref_ty_of_kind(&self, k: &RefKind, l: impl Upcast<Lt>) -> Self {
        let l: Lt = l.upcast();
        Self::rigid(k, vec![l.to::<Parameter>(), self.to::<Parameter>()])
    }

    pub fn bool() -> Ty {
        RigidTy {
            name: RigidName::ScalarId(ScalarId::Bool),
            parameters: vec![],
        }
        .upcast()
    }

    pub fn get_adt_id(&self) -> Option<AdtId> {
        if let Ty::RigidTy(rigid_ty) = self {
            if let RigidName::AdtId(ref adt_id) = rigid_ty.name {
                return Some(adt_id.clone());
            };
        };
        None
    }

    pub fn unit() -> Self {
        Ty::rigid(RigidName::Tuple(0), ())
    }
}

// ANCHOR: RigidTy_decl
#[term((rigid $name $*parameters))]
#[customize(parse, debug)]
pub struct RigidTy {
    pub name: RigidName,
    pub parameters: Parameters,
}
// ANCHOR_END: RigidTy_decl

impl UpcastFrom<ScalarId> for RigidTy {
    fn upcast_from(s: ScalarId) -> Self {
        RigidTy {
            name: s.upcast(),
            parameters: vec![],
        }
    }
}

impl DowncastTo<ScalarId> for RigidTy {
    fn downcast_to(&self) -> Option<ScalarId> {
        self.name.downcast_to()
    }
}

#[term]
pub enum RigidName {
    #[grammar((adt $v0))]
    #[cast]
    AdtId(AdtId),

    #[grammar((scalar $v0))]
    #[cast]
    ScalarId(ScalarId),

    #[cast]
    #[grammar(&($v0))]
    Ref(RefKind),

    #[cast]
    #[grammar(*$v0)]
    Raw(PtrKind),

    Tuple(usize),

    FnPtr(usize),

    FnDef(ValueId),

    #[grammar(!)]
    Never,
}

#[term]
#[derive(Copy, Default)]
pub enum RefKind {
    #[default]
    Shared,
    Mut,
}

#[term]
#[derive(Copy)]
pub enum PtrKind {
    Const,
    Mut,
}

#[term]
pub enum ScalarId {
    // FIXME: we should customize parsing here to permit e.g. `22_u8`
    #[grammar(u8)]
    U8,
    #[grammar(u16)]
    U16,
    #[grammar(u32)]
    U32,
    #[grammar(u64)]
    U64,
    #[grammar(i8)]
    I8,
    #[grammar(i16)]
    I16,
    #[grammar(i32)]
    I32,
    #[grammar(i64)]
    I64,
    #[grammar(bool)]
    Bool,
    #[grammar(usize)]
    Usize,
    #[grammar(isize)]
    Isize,
}

impl ScalarId {
    pub fn is_int(&self) -> bool {
        match self {
            ScalarId::U8
            | ScalarId::U16
            | ScalarId::U32
            | ScalarId::U64
            | ScalarId::I8
            | ScalarId::I16
            | ScalarId::I32
            | ScalarId::I64
            | ScalarId::Usize
            | ScalarId::Isize => true,
            ScalarId::Bool => false,
        }
    }
}

#[term((alias $name $*parameters))]
#[customize(parse, debug)]
pub struct AliasTy {
    pub name: AliasName,
    pub parameters: Parameters,
}

impl AliasTy {
    pub fn associated_ty(
        trait_id: impl Upcast<TraitId>,
        item_id: impl Upcast<AssociatedItemId>,
        item_arity: usize,
        parameters: impl Upcast<Vec<Parameter>>,
    ) -> Self {
        let parameters: Vec<Parameter> = parameters.upcast();
        assert!(item_arity <= parameters.len());
        AliasTy {
            name: AssociatedTyName {
                trait_id: trait_id.upcast(),
                item_id: item_id.upcast(),
                item_arity,
            }
            .upcast(),
            parameters,
        }
    }
}

#[term]
pub enum AliasName {
    #[cast]
    AssociatedTyId(AssociatedTyName),
}

#[term(($trait_id :: $item_id / $item_arity))]
pub struct AssociatedTyName {
    /// The trait in which the associated type was declared.
    pub trait_id: TraitId,

    /// The name of the associated type.
    pub item_id: AssociatedItemId,

    /// The number of parameters on the associated type (often 0).
    pub item_arity: usize,
}

#[term]
pub enum PredicateTy {
    ForAll(Binder<Arc<Ty>>),
}

#[term]
pub enum Parameter {
    #[cast]
    Ty(Arc<Ty>),
    #[cast]
    Lt(Arc<Lt>),
    #[cast]
    Const(Arc<Const>),
}

impl Parameter {
    pub fn kind(&self) -> ParameterKind {
        match self {
            Parameter::Ty(_) => ParameterKind::Ty,
            Parameter::Lt(_) => ParameterKind::Lt,
            Parameter::Const(_) => ParameterKind::Const,
        }
    }

    pub fn is_variable(&self) -> bool {
        self.as_variable().is_some()
    }

    pub fn as_variable(&self) -> Option<Variable> {
        match self {
            Parameter::Ty(v) => v.as_variable().copied(),
            Parameter::Lt(v) => v.as_variable().copied(),
            Parameter::Const(v) => v.as_variable().copied(),
        }
    }
}

pub type Parameters = Vec<Parameter>;

#[term]
#[derive(Copy)]
pub enum ParameterKind {
    Ty,
    Lt,
    Const,
}

#[term]
#[derive(Copy)]
pub enum Variance {
    #[grammar(+)]
    Covariant,
    #[grammar(-)]
    Contravariant,
    #[grammar(=)]
    Invariant,
}

#[term]
pub enum Lt {
    #[grammar('static)]
    Static,

    #[variable(ParameterKind::Lt)]
    Variable(Variable),
}

/// Temporary alias for migration.
pub type LtData = Lt;

impl Lt {
    pub fn static_() -> Self {
        Lt::Static
    }
}

impl UpcastFrom<Variable> for Parameter {
    fn upcast_from(v: Variable) -> Parameter {
        match v.kind() {
            ParameterKind::Lt => Lt::Variable(v.upcast()).upcast(),
            ParameterKind::Ty => Ty::Variable(v.upcast()).upcast(),
            ParameterKind::Const => Const::Variable(v.upcast()).upcast(),
        }
    }
}

impl DowncastTo<Variable> for Parameter {
    fn downcast_to(&self) -> Option<Variable> {
        self.as_variable()
    }
}

// Ty/Lt/Const <-> Parameter casts via the Arc<T> <-> T impls provided by #[term]
cast_impl!((Ty) <: (Arc<Ty>) <: (Parameter));
cast_impl!((Lt) <: (Arc<Lt>) <: (Parameter));
cast_impl!((Const) <: (Arc<Const>) <: (Parameter));

// Transitive casts
cast_impl!((RigidTy) <: (Ty) <: (Parameter));
cast_impl!((AliasTy) <: (Ty) <: (Parameter));
cast_impl!((ScalarId) <: (Ty) <: (Parameter));
cast_impl!((PredicateTy) <: (Ty) <: (Parameter));
cast_impl!((ScalarId) <: (RigidTy) <: (Ty));
// Variable -> Ty/Lt transitive casts
cast_impl!((UniversalVar) <: (Variable) <: (Ty));
cast_impl!((ExistentialVar) <: (Variable) <: (Ty));
cast_impl!((BoundVar) <: (Variable) <: (Ty));
cast_impl!((UniversalVar) <: (Variable) <: (Lt));
cast_impl!((ExistentialVar) <: (Variable) <: (Lt));
cast_impl!((BoundVar) <: (Variable) <: (Lt));
// Variable -> Parameter transitive casts
cast_impl!((UniversalVar) <: (Variable) <: (Parameter));
cast_impl!((ExistentialVar) <: (Variable) <: (Parameter));
cast_impl!((BoundVar) <: (Variable) <: (Parameter));
