use std::ops::Bound;

use bolero_generator::{Driver, TypeGenerator};
use formality_types::grammar::{AdtId, CrateId, Identifier, ParameterKind, TraitId};

use crate::grammar::{CrateItem, Program};

#[derive(TypeGenerator)]
pub struct ItemFuzzDecls {
    crates: Vec<DeclId<CrateId>>,
    traits: Vec<Decl<TraitId>>,
    structs: Vec<Decl<AdtId>>,
    enums: Vec<Decl<AdtId>>,
}

/// Generate a fresh id of type `I` and
/// a set of associated parameter kinds.
#[derive(TypeGenerator)]
struct Decl<I> {
    id: DeclId<I>,
    kinds: Vec<ParameterKind>,
}

#[derive(TypeGenerator)]
enum AdtKind {
    Struct,
    Enum,
}

/// Wrapper for id types (like `TraitId`, etc)
/// that, when generated, generates a fresh id,
/// rather than a reference to a set of other ids.
pub struct DeclId<I> {
    id: I,
}

impl<I> TypeGenerator for DeclId<I>
where
    I: From<String>,
{
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let s: String = driver.gen()?;
        Some(I::from(s))
    }
}

impl TypeGenerator for Program {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let kinds: Kinds = driver.gen()?;
        TraitId::fuzz_with_available_names()
    }
}

fn generate_traits(kinds: &Kinds) -> Vec<CrateItem> {
    kinds.traits.iter().map(|Kind { id, kinds }| {})
}
