use std::ops::{Bound, Range, RangeBounds};

use bolero_generator::{Driver, TypeGenerator};
use formality_types::{
    cast::Upcast,
    collections::Set,
    fuzz::{Fuzz, FuzzDriver, Fuzzer},
    grammar::{AdtId, AssociatedItemId, CrateId, ParameterKind, TraitId},
    set,
};

use crate::grammar::{Crate, CrateItem, Program, Trait};

impl TypeGenerator for Program {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let mut program_spec: ProgramSpec = driver.gen()?;

        program_spec.remove_dups();

        let fuzzer = &mut Fuzzer {
            driver: &mut BoleroFuzzDriver {
                bolero_driver: driver,
            },

            // for each trait
            traits: program_spec
                .trait_defs
                .iter()
                .map(|def| (def.id.id.clone(), def.kinds.kinds.clone()))
                .collect(),

            // for each adt
            adts: program_spec
                .adt_defs
                .iter()
                .map(|def| (def.id.id.clone(), def.kinds.kinds.clone()))
                .collect(),

            // for each associated type in each trait, create a mapping
            associated_types: program_spec
                .trait_defs
                .iter()
                .flat_map(|trait_def| {
                    trait_def.assoc_tys.iter().map(|assoc_ty| {
                        (
                            assoc_ty.id.id.clone(),
                            (
                                trait_def.id.id.clone(),
                                trait_def
                                    .kinds
                                    .kinds
                                    .iter()
                                    .chain(&assoc_ty.kinds.kinds)
                                    .cloned()
                                    .collect(),
                            ),
                        )
                    })
                })
                .collect(),
            variables: vec![],
        };

        // Create the random set of crates, always including a first crate named core
        let mut crates: Vec<Crate> = std::iter::once(Crate {
            id: CrateId::from("core"),
            items: vec![],
        })
        .chain(program_spec.addl_crate_ids.iter().map(|id| Crate {
            id: id.id.clone(),
            items: vec![],
        }))
        .collect();

        for trait_def in &program_spec.trait_defs {
            let d = Trait::fuzz(fuzzer)?;
            distribute_to_crate(fuzzer, d, &mut crates)?;
        }

        Ok(())
    }
}

fn distribute_to_crate(
    fuzzer: &mut Fuzzer<'_>,
    item: impl Upcast<CrateItem>,
    crates: &mut Vec<Crate>,
) -> Option<()> {
    let crate_index = fuzzer.gen_usize(0..crates.len())?;
    crates[crate_index].items.push(item.upcast());
}

struct BoleroFuzzDriver<'d, D: Driver> {
    bolero_driver: &'d mut D,
}

impl<D> FuzzDriver for BoleroFuzzDriver<'_, D>
where
    D: Driver,
{
    fn gen_usize(&mut self, range: Range<usize>) -> Option<usize> {
        self.bolero_driver
            .gen_usize(range.start_bound(), range.end_bound())
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, TypeGenerator)]
struct ProgramSpec {
    addl_crate_ids: Vec<Id<CrateId>>,
    trait_defs: Vec<TraitDef>,
    adt_defs: Vec<AdtDef>,
}

impl ProgramSpec {
    /// After initial generation, there may be duplicate trait-ids or adt-ids.
    /// It's unlikely, but possible. Fix it by removing duplicates in some deterministic way.
    fn remove_dups(&mut self) {
        remove_dups_by_name(&mut self.addl_crate_ids, |d| d);
        for trait_def in &mut self.trait_defs {
            remove_dups_by_name(&mut trait_def.assoc_tys, |d| &d.id);
        }
        remove_dups_by_name(&mut self.trait_defs, |d| &d.id);
        remove_dups_by_name(&mut self.adt_defs, |d| &d.id);
    }
}

fn remove_dups_by_name<T, I>(v: &mut Vec<T>, name: impl Fn(&T) -> &I)
where
    I: Ord + Clone,
{
    let mut set: Set<I> = set![];
    v.retain(|e| set.insert(name(e).clone()));
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, TypeGenerator)]
struct TraitDef {
    id: Id<TraitId>,
    kinds: Kinds,
    assoc_tys: Vec<AssocTyDef>,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, TypeGenerator)]
struct AssocTyDef {
    id: Id<AssociatedItemId>,
    kinds: Kinds,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, TypeGenerator)]
struct AdtDef {
    id: Id<AdtId>,
    kinds: Kinds,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Id<I> {
    id: I,
}

trait FuzzableId: From<String> {
    const PREFIX: &'static str;
}

impl<I> TypeGenerator for Id<I>
where
    I: FuzzableId,
{
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let id = driver.gen_usize(Bound::Unbounded, Bound::Unbounded)?;
        Some(Id {
            id: I::from(format!("{}{id}", I::PREFIX)),
        })
    }
}

impl FuzzableId for TraitId {
    const PREFIX: &'static str = "Trait";
}

impl FuzzableId for AdtId {
    const PREFIX: &'static str = "Adt";
}

impl FuzzableId for AssociatedItemId {
    const PREFIX: &'static str = "Assoc";
}

impl FuzzableId for CrateId {
    const PREFIX: &'static str = "Crate";
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Kinds {
    kinds: Vec<ParameterKind>,
}

impl TypeGenerator for Kinds {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        const MAX_ITEM_ARITY: usize = 3;
        let driver: &dyn FuzzDriver = driver;
        let arity = driver.gen_usize(0..MAX_ITEM_ARITY)?;
        let kinds: Vec<ParameterKind> = (0..arity)
            .map(|_| driver.pick(ParameterKind::variants().iter()))
            .collect::<Option<_>>()?;
        Some(Kinds { kinds })
    }
}

impl Fuzz for Trait {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        todo!()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        todo!()
    }
}
