use std::ops::{Bound, Range, RangeBounds};

use bolero_generator::{Driver, TypeGenerator};
use formality_types::{
    cast::{Downcast, Upcast, Upcasted},
    collections::Set,
    fuzz::{Fuzz, FuzzDriver, Fuzzer, PickVariant},
    grammar::{
        AdtId, AssociatedItemId, Binder, CrateId, FieldId, Lt, Parameter, ParameterKind, TraitId,
        TraitRef, Ty, VariantId,
    },
    set,
};

use crate::grammar::{
    AdtBoundData, AssociatedTy, AssociatedTyBoundData, Crate, CrateItem, Enum, Field, FieldName,
    Program, Struct, StructBoundData, Trait, TraitBinder, TraitBoundData, Variant, WhereBound,
    WhereBoundData, WhereClause, WhereClauseData,
};

impl TypeGenerator for Program {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let mut program_spec: ProgramSpec = driver.gen()?;

        program_spec.postprocess();

        let fuzzer = &mut Fuzzer {
            driver: &mut BoleroFuzzDriver {
                bolero_driver: driver,
            },

            // for each trait
            traits: program_spec
                .trait_defs
                .iter()
                .map(|def| (def.id.id(), def.kinds.kinds.clone()))
                .collect(),

            // for each adt
            adts: program_spec
                .adt_defs
                .iter()
                .map(|def| (def.id.id(), def.kinds.kinds.clone()))
                .collect(),

            // for each associated type in each trait, create a mapping
            associated_types: program_spec
                .trait_defs
                .iter()
                .flat_map(|trait_def| {
                    trait_def.assoc_tys.iter().map(|assoc_ty| {
                        (
                            assoc_ty.id.id(),
                            (
                                trait_def.id.id(),
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

            field_ids: program_spec.field_ids.iter().map(|f| f.id()).collect(),

            variant_ids: program_spec.variant_ids.iter().map(|f| f.id()).collect(),
        };

        // Create the random set of crates, always including a first crate named core
        let mut crates: Vec<Crate> = std::iter::once(Crate {
            id: CrateId::from("core"),
            items: vec![],
        })
        .chain(program_spec.addl_crate_ids.iter().map(|id| Crate {
            id: id.id(),
            items: vec![],
        }))
        .collect();

        for trait_def in &program_spec.trait_defs {
            let d = trait_def.fuzz(fuzzer)?;
            distribute_to_crate(fuzzer, d, &mut crates)?;
        }

        for adt_def in &program_spec.adt_defs {
            let d = adt_def.fuzz(fuzzer)?;
            distribute_to_crate(fuzzer, d, &mut crates)?;
        }

        Some(Program { crates })
    }
}

fn distribute_to_crate(
    fuzzer: &mut Fuzzer<'_>,
    item: impl Upcast<CrateItem>,
    crates: &mut Vec<Crate>,
) -> Option<()> {
    let crate_index = fuzzer.gen_usize(0..crates.len())?;
    crates[crate_index].items.push(item.upcast());
    Some(())
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
    field_ids: Vec<Id<FieldId>>,
    variant_ids: Vec<Id<VariantId>>,
}

impl ProgramSpec {
    /// After initial generation, there may be duplicate trait-ids or adt-ids.
    /// It's unlikely, but possible. Fix it by removing duplicates in some deterministic way.
    fn postprocess(&mut self) {
        remove_dups_by_name(&mut self.addl_crate_ids, |d| d);
        for trait_def in &mut self.trait_defs {
            // ensure that every trait has a Self type parameter
            trait_def.kinds.kinds.insert(0, ParameterKind::Ty);

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

impl TraitDef {
    fn fuzz(&self, fuzzer: &mut Fuzzer<'_>) -> Option<Trait> {
        Some(Trait {
            id: self.id.id(),
            binder: TraitBinder {
                explicit_binder: fuzzer.binder(&self.kinds.kinds, |fuzzer| {
                    let assoc_trait_items = self
                        .assoc_tys
                        .iter()
                        .map(|a| {
                            Some(AssociatedTy {
                                id: a.id.id(),
                                binder: fuzzer.binder(&a.kinds.kinds, Fuzz::fuzz)?,
                            })
                        })
                        .collect::<Option<Vec<_>>>()?;

                    let fn_trait_items: Vec<crate::grammar::Fn> = Fuzz::fuzz(fuzzer)?;

                    Some(TraitBoundData {
                        where_clauses: Fuzz::fuzz(fuzzer)?,
                        trait_items: assoc_trait_items
                            .into_iter()
                            .upcasted()
                            .chain(fn_trait_items.into_iter().upcasted())
                            .collect(),
                    })
                })?,
            },
        })
    }
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

impl AdtDef {
    fn fuzz(&self, fuzzer: &mut Fuzzer<'_>) -> Option<CrateItem> {
        PickVariant::new(fuzzer)
            .variant(
                |_| true,
                |fuzzer| {
                    Some(Struct {
                        id: self.id.id(),
                        binder: fuzzer.binder(&self.kinds.kinds, Fuzz::fuzz)?,
                    })
                },
            )
            .variant(
                |_| true,
                |fuzzer| {
                    Some(Enum {
                        id: self.id.id(),
                        binder: fuzzer.binder(&self.kinds.kinds, Fuzz::fuzz)?,
                    })
                },
            )
            .finish()
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Id<I> {
    id: I,
}

impl<I> Id<I>
where
    I: Clone,
{
    fn id(&self) -> I {
        self.id.clone()
    }
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

impl FuzzableId for FieldId {
    const PREFIX: &'static str = "Field";
}

impl FuzzableId for VariantId {
    const PREFIX: &'static str = "Variant";
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Kinds {
    kinds: Vec<ParameterKind>,
}

impl TypeGenerator for Kinds {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        const MAX_ITEM_ARITY: usize = 3;
        let driver: &mut dyn FuzzDriver = &mut BoleroFuzzDriver {
            bolero_driver: driver,
        };
        let arity = driver.gen_usize(0..MAX_ITEM_ARITY)?;
        let kinds: Vec<ParameterKind> = (0..arity)
            .map(|_| driver.pick(ParameterKind::variants().iter()))
            .collect::<Option<_>>()?;
        Some(Kinds { kinds })
    }
}

impl Fuzz for AssociatedTyBoundData {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        Some(AssociatedTyBoundData {
            ensures: Fuzz::fuzz(fuzzer)?,
            where_clauses: Fuzz::fuzz(fuzzer)?,
        })
    }
}

impl Fuzz for WhereBound {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        TraitRef::inhabited(fuzzer)
            || Lt::inhabited(fuzzer)
            || <Binder<WhereBound>>::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(TraitRef::inhabited, |fuzzer| {
                let TraitRef {
                    trait_id,
                    mut parameters,
                } = TraitRef::fuzz(fuzzer)?;
                parameters.remove(0);
                Some(WhereBoundData::IsImplemented(trait_id, parameters))
            })
            .variant(Lt::inhabited, |fuzzer| {
                Some(WhereBoundData::Outlives(Fuzz::fuzz(fuzzer)?))
            })
            .variant(<Binder<WhereBound>>::inhabited, |fuzzer| {
                Some(WhereBoundData::ForAll(Fuzz::fuzz(fuzzer)?))
            })
            .finish()
    }
}

impl Fuzz for WhereClause {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(TraitRef::inhabited, |fuzzer| {
                let TraitRef {
                    trait_id,
                    mut parameters,
                } = TraitRef::fuzz(fuzzer)?;
                let self_ty: Ty = parameters.remove(0).downcast().unwrap();
                Some(WhereClauseData::IsImplemented(
                    self_ty, trait_id, parameters,
                ))
            })
            .variant(
                |_| true,
                |fuzzer| {
                    let (p, lt): (Parameter, Lt) = Fuzz::fuzz(fuzzer)?;
                    Some(WhereClauseData::Outlives(p, lt))
                },
            )
            .variant(
                |_| true,
                |fuzzer| Some(WhereClauseData::ForAll(Fuzz::fuzz(fuzzer)?)),
            )
            .finish()
    }
}

impl Fuzz for StructBoundData {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        Some(StructBoundData {
            where_clauses: Fuzz::fuzz(fuzzer)?,
            fields: Fuzz::fuzz(fuzzer)?,
        })
    }
}

impl Fuzz for Field {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        Some(Field {
            name: Fuzz::fuzz(fuzzer)?,
            ty: Fuzz::fuzz(fuzzer)?,
        })
    }
}

impl Fuzz for FieldName {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(
                |_| true,
                |fuzzer| Some(FieldName::Index(fuzzer.gen_usize(0..usize::MAX)?)),
            )
            .variant(FieldId::inhabited, FieldId::fuzz)
            .finish()
    }
}

impl Fuzz for AdtBoundData {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        Some(AdtBoundData {
            where_clauses: Fuzz::fuzz(fuzzer)?,
            variants: Fuzz::fuzz(fuzzer)?,
        })
    }
}

impl Fuzz for Variant {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        Some(Variant {
            name: Fuzz::fuzz(fuzzer)?,
            fields: Fuzz::fuzz(fuzzer)?,
        })
    }
}

impl Fuzz for crate::grammar::Fn {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        false
    }

    fn fuzz(_fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        todo!()
    }
}
