use crate::grammar::{
    Adt, AdtBoundData, AssociatedTy, AssociatedTyBoundData, AssociatedTyValue,
    AssociatedTyValueBoundData, Crate, CrateItem, Fn, FnBoundData, ImplItem, NegTraitImpl,
    NegTraitImplBoundData, Program, Trait, TraitBoundData, TraitImpl, TraitImplBoundData,
    TraitItem, WhereBound, WhereBoundData, WhereClause, WhereClauseData,
};
use formality_core::{seq, Set, To, Upcast, Upcasted};
use formality_prove as prove;
use formality_types::grammar::{
    AdtId, AliasTy, Binder, BoundVar, ParameterKind, Predicate, Relation, TraitId, Ty, Wc, Wcs,
};

impl Program {
    pub fn to_prove_decls(&self) -> prove::Decls {
        formality_prove::Decls {
            max_size: formality_prove::Decls::DEFAULT_MAX_SIZE,
            trait_decls: self.trait_decls(),
            impl_decls: self.impl_decls(),
            neg_impl_decls: self.neg_impl_decls(),
            alias_eq_decls: self.alias_eq_decls(),
            alias_bound_decls: self.alias_bound_decls(),
            adt_decls: self.adt_decls(),
            fn_decls: self.fn_decls(),
            local_trait_ids: self.local_trait_ids(),
            local_adt_ids: self.local_adt_ids(),
        }
    }

    fn trait_decls(&self) -> Vec<prove::TraitDecl> {
        self.crates.iter().flat_map(|c| c.trait_decls()).collect()
    }

    fn impl_decls(&self) -> Vec<prove::ImplDecl> {
        self.crates.iter().flat_map(|c| c.impl_decls()).collect()
    }

    fn neg_impl_decls(&self) -> Vec<prove::NegImplDecl> {
        self.crates
            .iter()
            .flat_map(|c| c.neg_impl_decls())
            .collect()
    }

    fn alias_eq_decls(&self) -> Vec<prove::AliasEqDecl> {
        self.crates
            .iter()
            .flat_map(|c| c.alias_eq_decls())
            .collect()
    }

    fn alias_bound_decls(&self) -> Vec<prove::AliasBoundDecl> {
        self.crates
            .iter()
            .flat_map(|c| c.alias_bound_decls())
            .collect()
    }

    fn adt_decls(&self) -> Vec<prove::AdtDecl> {
        self.crates.iter().flat_map(|c| c.adt_decls()).collect()
    }

    fn fn_decls(&self) -> Vec<prove::FnDecl> {
        self.crates.iter().flat_map(|c| c.fn_decls()).collect()
    }

    fn local_trait_ids(&self) -> Set<TraitId> {
        self.crates
            .last()
            .into_iter()
            .flat_map(|c| c.trait_decls().into_iter().map(|decl| decl.id))
            .collect()
    }

    fn local_adt_ids(&self) -> Set<AdtId> {
        self.crates
            .last()
            .into_iter()
            .flat_map(|c| c.adt_ids())
            .collect()
    }
}

impl Crate {
    fn trait_decls(&self) -> Vec<prove::TraitDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::Trait(Trait { id, binder, safety }) => {
                    let (
                        vars,
                        TraitBoundData {
                            where_clauses,
                            trait_items: _,
                        },
                    ) = binder.open();
                    Some(prove::TraitDecl {
                        safety: safety.clone(),
                        id: id.clone(),
                        binder: Binder::new(
                            vars,
                            prove::TraitDeclBoundData {
                                where_clause: where_clauses
                                    .iter()
                                    .flat_map(|wc| wc.to_wcs())
                                    .collect(),
                            },
                        ),
                    })
                }
                _ => None,
            })
            .collect()
    }

    fn impl_decls(&self) -> Vec<prove::ImplDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::TraitImpl(TraitImpl { binder, safety }) => {
                    let (
                        vars,
                        TraitImplBoundData {
                            trait_id,
                            self_ty,
                            trait_parameters,
                            where_clauses,
                            impl_items: _,
                        },
                    ) = binder.open();
                    Some(prove::ImplDecl {
                        safety: safety.clone(),
                        binder: Binder::new(
                            vars,
                            prove::ImplDeclBoundData {
                                trait_ref: trait_id.with(self_ty, trait_parameters),
                                where_clause: where_clauses.to_wcs(),
                            },
                        ),
                    })
                }
                _ => None,
            })
            .collect()
    }

    fn neg_impl_decls(&self) -> Vec<prove::NegImplDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::NegTraitImpl(NegTraitImpl { binder, safety }) => {
                    let (
                        vars,
                        NegTraitImplBoundData {
                            trait_id,
                            self_ty,
                            trait_parameters,
                            where_clauses,
                        },
                    ) = binder.open();
                    Some(prove::NegImplDecl {
                        safety: safety.clone(),
                        binder: Binder::new(
                            vars,
                            prove::NegImplDeclBoundData {
                                trait_ref: trait_id.with(self_ty, trait_parameters),
                                where_clause: where_clauses.to_wcs(),
                            },
                        ),
                    })
                }
                _ => None,
            })
            .collect()
    }

    fn alias_eq_decls(&self) -> Vec<prove::AliasEqDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::TraitImpl(TraitImpl { binder, safety: _ }) => {
                    let (
                        impl_vars,
                        TraitImplBoundData {
                            trait_id,
                            self_ty,
                            trait_parameters,
                            where_clauses: impl_wc,
                            impl_items,
                        },
                    ) = binder.open();

                    Vec::from_iter(impl_items.iter().flat_map(|impl_item| match impl_item {
                        ImplItem::Fn(_) => None,
                        ImplItem::AssociatedTyValue(AssociatedTyValue {
                            id: item_id,
                            binder,
                        }) => {
                            let (
                                assoc_vars,
                                AssociatedTyValueBoundData {
                                    where_clauses: assoc_wc,
                                    ty,
                                },
                            ) = binder.open();
                            Some(prove::AliasEqDecl {
                                binder: Binder::new(
                                    (&impl_vars, &assoc_vars),
                                    prove::AliasEqDeclBoundData {
                                        alias: AliasTy::associated_ty(
                                            &trait_id,
                                            item_id,
                                            assoc_vars.len(),
                                            seq![
                                                self_ty.to(),
                                                ..trait_parameters.iter().cloned(),
                                                ..assoc_vars.iter().upcasted(),
                                            ],
                                        ),
                                        ty,
                                        where_clause: (&impl_wc, assoc_wc).to_wcs(),
                                    },
                                ),
                            })
                        }
                    }))
                }
                _ => vec![],
            })
            .collect()
    }

    fn alias_bound_decls(&self) -> Vec<prove::AliasBoundDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::Trait(Trait {
                    safety: _,
                    id: trait_id,
                    binder,
                }) => {
                    let (
                        trait_vars,
                        TraitBoundData {
                            where_clauses: trait_wc,
                            trait_items,
                        },
                    ) = binder.open();

                    Vec::from_iter(trait_items.iter().flat_map(|trait_item| match trait_item {
                        TraitItem::Fn(_) => vec![],
                        TraitItem::AssociatedTy(AssociatedTy {
                            id: item_id,
                            binder,
                        }) => {
                            let (
                                assoc_vars,
                                AssociatedTyBoundData {
                                    ensures,
                                    where_clauses: assoc_wc,
                                },
                            ) = binder.open();
                            let alias = AliasTy::associated_ty(
                                trait_id,
                                item_id,
                                assoc_vars.len(),
                                (&trait_vars, &assoc_vars),
                            );

                            ensures
                                .iter()
                                .map(|e| {
                                    let fresh_var = BoundVar::fresh(ParameterKind::Ty);
                                    let ensures = Binder::new(vec![fresh_var], e.to_wc(fresh_var));

                                    prove::AliasBoundDecl {
                                        binder: Binder::new(
                                            (&trait_vars, &assoc_vars),
                                            prove::AliasBoundDeclBoundData {
                                                alias: alias.clone(),
                                                ensures,
                                                where_clause: (&trait_wc, &assoc_wc).to_wcs(),
                                            },
                                        ),
                                    }
                                })
                                .collect::<Vec<_>>()
                        }
                    }))
                }
                _ => vec![],
            })
            .collect()
    }

    fn adt_decls(&self) -> Vec<prove::AdtDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::Struct(s) => Some(s.to_adt()),
                CrateItem::Enum(e) => Some(e.to_adt()),
                _ => None,
            })
            .map(|Adt { id, binder }| {
                let (
                    vars,
                    AdtBoundData {
                        where_clauses,
                        variants: _,
                    },
                ) = binder.open();
                prove::AdtDecl {
                    id: id.clone(),
                    binder: Binder::new(
                        vars,
                        prove::AdtDeclBoundData {
                            where_clause: where_clauses.iter().flat_map(|wc| wc.to_wcs()).collect(),
                        },
                    ),
                }
            })
            .collect()
    }

    fn adt_ids(&self) -> Set<AdtId> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::Struct(v) => Some(v.id.clone()),
                CrateItem::Enum(v) => Some(v.id.clone()),
                CrateItem::Trait(_) => None,
                CrateItem::TraitImpl(_) => None,
                CrateItem::NegTraitImpl(_) => None,
                CrateItem::Fn(_) => None,
                CrateItem::Test(_) => None,
            })
            .collect()
    }

    fn fn_decls(&self) -> Vec<prove::FnDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::Fn(f) => Some(f),
                _ => None,
            })
            .map(|Fn { id, binder }| prove::FnDecl {
                id: id.clone(),
                binder: binder.map(
                    |FnBoundData {
                         input_tys,
                         output_ty,
                         where_clauses,
                         body: _,
                     }| prove::FnDeclBoundData {
                        input_tys,
                        output_ty,
                        where_clause: where_clauses.to_wcs(),
                    },
                ),
            })
            .collect()
    }
}

pub trait ToWcs {
    fn to_wcs(&self) -> Wcs;
}

impl<T: ?Sized + ToWcs> ToWcs for &T {
    fn to_wcs(&self) -> Wcs {
        T::to_wcs(self)
    }
}

macro_rules! upcast_to_wcs {
    ($($t:ty,)*) => {
        $(
            impl ToWcs for $t {
                fn to_wcs(&self) -> Wcs {
                    self.upcast()
                }
            }
        )*
    }
}

upcast_to_wcs! {
    Wc,
    Wcs,
    Predicate,
    Relation,
}

impl ToWcs for () {
    fn to_wcs(&self) -> Wcs {
        Wcs::t()
    }
}

impl<A, B> ToWcs for (A, B)
where
    A: ToWcs,
    B: ToWcs,
{
    fn to_wcs(&self) -> Wcs {
        let (a, b) = self;
        let a = a.to_wcs();
        let b = b.to_wcs();
        (a, b).upcast()
    }
}

impl<A, B, C> ToWcs for (A, B, C)
where
    A: ToWcs,
    B: ToWcs,
    C: ToWcs,
{
    fn to_wcs(&self) -> Wcs {
        let (a, b, c) = self;
        let a = a.to_wcs();
        let b = b.to_wcs();
        let c = c.to_wcs();
        (a, b, c).upcast()
    }
}
impl ToWcs for Vec<WhereClause> {
    fn to_wcs(&self) -> Wcs {
        self.iter().flat_map(|wc| wc.to_wcs()).collect()
    }
}

impl ToWcs for [WhereClause] {
    fn to_wcs(&self) -> Wcs {
        self.iter().flat_map(|wc| wc.to_wcs()).collect()
    }
}

impl ToWcs for WhereClause {
    fn to_wcs(&self) -> Wcs {
        match self.data() {
            WhereClauseData::IsImplemented(self_ty, trait_id, parameters) => {
                trait_id.with(self_ty, parameters).upcast()
            }
            WhereClauseData::AliasEq(alias_ty, ty) => {
                Predicate::AliasEq(alias_ty.clone(), ty.clone()).upcast()
            }
            WhereClauseData::Outlives(a, b) => Relation::outlives(a, b).upcast(),
            WhereClauseData::ForAll(binder) => {
                let (vars, wc) = binder.open();
                wc.to_wcs()
                    .into_iter()
                    .map(|wc| Wc::for_all(&vars, wc))
                    .collect()
            }
            WhereClauseData::TypeOfConst(ct, ty) => {
                Predicate::ConstHasType(ct.clone(), ty.clone()).upcast()
            }
        }
    }
}

impl WhereBound {
    pub fn to_wc(&self, self_ty: impl Upcast<Ty>) -> Wc {
        let self_ty: Ty = self_ty.upcast();

        match self.data() {
            WhereBoundData::IsImplemented(trait_id, parameters) => {
                trait_id.with(self_ty, parameters).upcast()
            }
            WhereBoundData::Outlives(lt) => Relation::outlives(self_ty, lt).upcast(),
            WhereBoundData::ForAll(binder) => {
                let (vars, bound) = binder.open();
                Wc::for_all(&vars, bound.to_wc(self_ty))
            }
        }
    }
}
