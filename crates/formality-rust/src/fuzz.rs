//! Generate "mostly valid" Rust programs.
//!
//! See the Formality Book [chapter on fuzzing][f] for more details.
//!
//! [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html

use bolero::Driver;
use formality_core::{
    binder::{fresh_bound_var, fuzz::KindVec},
    fuzz::FuzzConfig,
    Fuzz, Map, Upcast,
};
use formality_types::{
    fuzz::RustTypesFuzzConfig,
    grammar::{AdtId, Binder, CrateId, ParameterKind, TraitId, Variable},
    rust::FormalityLang,
};

use crate::grammar::{AdtBoundData, Crate, CrateItem, Enum, Program, Struct, StructBoundData};

#[derive(Debug)]
pub struct FuzzProgram {
    pub num_adts: std::ops::Range<usize>,
    pub num_traits: std::ops::Range<usize>,
    pub num_generic_parameters: std::ops::Range<usize>,
}

impl FuzzProgram {
    pub fn num_adts(self, r: std::ops::Range<usize>) -> Self {
        Self {
            num_adts: r,
            ..self
        }
    }

    pub fn num_traits(self, r: std::ops::Range<usize>) -> Self {
        Self {
            num_traits: r,
            ..self
        }
    }

    pub fn num_generic_parameters(self, r: std::ops::Range<usize>) -> Self {
        Self {
            num_generic_parameters: r,
            ..self
        }
    }
}

impl Default for FuzzProgram {
    fn default() -> Self {
        Self {
            num_adts: (0..10),
            num_traits: (0..10),
            num_generic_parameters: (0..3),
        }
    }
}

/// Set of items that we wll declare in our program.
/// We begin by fuzzing an instance of this type.
#[derive(Fuzz, Debug)]
struct FuzzItems {
    adts: Vec<FuzzAdt>,
    traits: Vec<FuzzTrait>,
}

/// An ADT that will be declared along with its parameter kinds.
/// Its name uses a `String` so that the fuzzer can generate arbitrary names.
#[derive(Fuzz, Debug)]
struct FuzzAdt {
    arity: KindVec<FormalityLang>,
}

/// A trait that will be declared along with its parameter kinds.
/// Its name uses a `String` so that the fuzzer can generate arbitrary names.
#[derive(Fuzz, Debug)]
struct FuzzTrait {
    arity: KindVec<FormalityLang>,
}

/// The bound data for an ADT definition (fuzzer can choose between struct/enum).
#[derive(Fuzz)]
enum FuzzAdtBoundData {
    Struct(StructBoundData),
    Enum(AdtBoundData),
}

impl bolero::ValueGenerator for FuzzProgram {
    type Output = Program;

    fn generate<D: Driver>(&self, driver: &mut D) -> Option<Self::Output> {
        let mut cfg = FuzzConfig::new().with_binder_range(self.num_generic_parameters.clone());

        // First we determine what kind of items there will be.
        let items = {
            let mut cx = cfg.clone().into_cx(&mut *driver);
            FuzzItems {
                adts: cx.fuzz_many(self.num_adts.clone())?,
                traits: cx.fuzz_many(self.num_traits.clone())?,
            }
        };

        // Then bring those items into scope for what follows.
        let adt_kinds: Map<AdtId, Vec<ParameterKind>> = items
            .adts
            .into_iter()
            .zip(0..)
            .map(|(f, idx)| (AdtId::new(&format!("Adt{}", idx)), f.arity.into_vec()))
            .collect();
        let trait_kinds: Map<TraitId, Vec<ParameterKind>> = items
            .traits
            .into_iter()
            .zip(0..)
            .map(|(f, idx)| (TraitId::new(&format!("Trait{}", idx)), f.arity.into_vec()))
            .collect();
        cfg = cfg.with_rust_types(adt_kinds.clone(), trait_kinds.clone(), Map::default());

        // Now generate the bodies of the type declarations.
        let adt_decls: Vec<CrateItem> = adt_kinds
            .iter()
            .map(|(adt_id, kinds)| -> Option<CrateItem> {
                let variables: Vec<Variable> = kinds
                    .iter()
                    .map(|&kind| fresh_bound_var(kind).upcast())
                    .collect();
                let mut cx = cfg
                    .clone()
                    .with_free_variables(variables.clone())
                    .into_cx(&mut *driver);

                let adt_decl: FuzzAdtBoundData = cx.fuzz()?;
                match adt_decl {
                    FuzzAdtBoundData::Struct(s) => Some(
                        Struct {
                            id: adt_id.clone(),
                            binder: Binder::new(variables, s),
                        }
                        .upcast(),
                    ),
                    FuzzAdtBoundData::Enum(s) => Some(
                        Enum {
                            id: adt_id.clone(),
                            binder: Binder::new(variables, s),
                        }
                        .upcast(),
                    ),
                }
            })
            .collect::<Option<_>>()?;

        // Now generate the bodies of the traits.
        let trait_decls: Vec<CrateItem> = vec![/*TODO*/];

        // Now wrap it up in a crate.
        let krate = Crate {
            id: CrateId::new("fuzz"),
            items: adt_decls.into_iter().chain(trait_decls).collect(),
        };

        Some(Program {
            crates: vec![krate],
        })
    }
}
