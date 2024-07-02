//! Generate "mostly valid" Rust programs.
//!
//! See the Formality Book [chapter on fuzzing][f] for more details.
//!
//! [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html

use bolero::Driver;
use formality_core::{language::Language, Map, Upcast};
use formality_types::{
    fuzz::FuzzCx,
    grammar::{AdtId, CrateId, ParameterKind, TraitId},
};

use crate::grammar::{AdtBoundData, Crate, CrateItem, Enum, Program, Struct, StructBoundData};

/// Set of items that we wll declare in our program.
/// We begin by fuzzing an instance of this type.
#[derive(bolero::TypeGenerator)]
struct FuzzItems {
    adts: Vec<FuzzAdt>,
    traits: Vec<FuzzTrait>,
}

/// An ADT that will be declared along with its parameter kinds.
/// Its name uses a `String` so that the fuzzer can generate arbitrary names.
#[derive(bolero::TypeGenerator)]
struct FuzzAdt {
    name: String,
    arity: Vec<ParameterKind>,
}

/// A trait that will be declared along with its parameter kinds.
/// Its name uses a `String` so that the fuzzer can generate arbitrary names.
#[derive(bolero::TypeGenerator)]
struct FuzzTrait {
    name: String,
    arity: Vec<ParameterKind>,
}

/// The bound data for an ADT definition (fuzzer can choose between struct/enum).
#[derive(bolero::TypeGenerator)]
enum FuzzAdtBoundData {
    Struct(StructBoundData),
    Enum(AdtBoundData),
}

impl bolero::TypeGenerator for Program {
    fn generate<D: Driver>(driver: &mut D) -> Option<Program> {
        // First we determine what kind of items there will be.
        let items: FuzzItems = driver.gen()?;

        // Then bring those items into scope for what follows.
        let _guard = FuzzCx {
            adt_kinds: items
                .adts
                .into_iter()
                .map(|f| (AdtId::new(&f.name), f.arity))
                .collect(),
            trait_kinds: items
                .traits
                .into_iter()
                .map(|f| (TraitId::new(&f.name), f.arity))
                .collect(),
            associated_items: Map::default(),
        }
        .install();

        // Now generate the bodies of the type declarations.
        let adt_decls: Vec<CrateItem> = FuzzCx::adt_id_map()
            .get()
            .iter()
            .map(|(adt_id, kinds)| -> Option<CrateItem> {
                let guard = crate::FormalityLang::open_fuzz_binder(kinds);
                let adt_decl: FuzzAdtBoundData = driver.gen()?;
                match adt_decl {
                    FuzzAdtBoundData::Struct(s) => Some(
                        Struct {
                            id: adt_id.clone(),
                            binder: guard.into_binder(s),
                        }
                        .upcast(),
                    ),
                    FuzzAdtBoundData::Enum(s) => Some(
                        Enum {
                            id: adt_id.clone(),
                            binder: guard.into_binder(s),
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
