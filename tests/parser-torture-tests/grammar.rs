use crate::ptt::{
    grammar::{BoundVar, ExistentialVar, UniversalVar, Variable},
    FormalityLang,
};
use formality_core::{language::HasKind, term};

// Create a dummy kind/parameter -- we're not using these for the torture
// tests, but we need them.

#[term]
#[derive(Copy)]
pub enum DummyKind {
    Ty,
}

#[term]
pub enum DummyParameter {
    #[cast]
    Ty(DummyTy),
}

#[term]
pub enum DummyTy {
    #[variable]
    Variable(Variable),
}

formality_core::cast_impl!((BoundVar) <: (Variable) <: (DummyTy));
formality_core::cast_impl!((ExistentialVar) <: (Variable) <: (DummyTy));
formality_core::cast_impl!((UniversalVar) <: (Variable) <: (DummyTy));
formality_core::cast_impl!((Variable) <: (DummyTy) <: (DummyParameter));
formality_core::cast_impl!((BoundVar) <: (DummyTy) <: (DummyParameter));
formality_core::cast_impl!((ExistentialVar) <: (DummyTy) <: (DummyParameter));
formality_core::cast_impl!((UniversalVar) <: (DummyTy) <: (DummyParameter));

impl HasKind<FormalityLang> for DummyParameter {
    fn kind(&self) -> formality_core::language::CoreKind<FormalityLang> {
        match self {
            DummyParameter::Ty(_) => DummyKind::Ty,
        }
    }
}
