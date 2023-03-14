use crate::grammar::{
    AssociatedTy, AssociatedTyBoundData, AssociatedTyValue, AssociatedTyValueBoundData, Crate,
    CrateItem, ImplItem, Program, Trait, TraitBoundData, TraitImpl, TraitImplBoundData, TraitItem,
    WhereBound, WhereBoundData, WhereClause, WhereClauseData,
};
use formality_prove as prove;
use formality_types::{
    cast::{To, Upcast, Upcasted},
    grammar::{
        fresh_bound_var, AliasTy, AtomicRelation, Binder, ParameterKind, Predicate, Ty, Wc, Wcs,
        APR,
    },
    seq,
};

impl Program {
    pub fn to_prove_decls(&self) -> prove::Decls {
        formality_prove::Decls {
            max_size: formality_prove::Decls::DEFAULT_MAX_SIZE,
            trait_decls: self.trait_decls(),
            impl_decls: self.impl_decls(),
            alias_eq_decls: self.alias_eq_decls(),
            alias_bound_decls: self.alias_bound_decls(),
        }
    }

    fn trait_decls(&self) -> Vec<prove::TraitDecl> {
        self.crates.iter().flat_map(|c| c.trait_decls()).collect()
    }

    fn impl_decls(&self) -> Vec<prove::ImplDecl> {
        self.crates.iter().flat_map(|c| c.impl_decls()).collect()
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
}

impl Crate {
    fn trait_decls(&self) -> Vec<prove::TraitDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::Trait(Trait { id, binder }) => {
                    let (
                        vars,
                        TraitBoundData {
                            where_clauses,
                            trait_items: _,
                        },
                    ) = binder.open();
                    Some(prove::TraitDecl {
                        id: id.clone(),
                        binder: Binder::new(
                            &vars,
                            prove::TraitDeclBoundData {
                                where_clause: where_clauses
                                    .iter()
                                    .flat_map(|wc| wc.to_wcs())
                                    .collect(),
                            },
                        ),
                    })
                }
                CrateItem::Struct(_)
                | CrateItem::Enum(_)
                | CrateItem::TraitImpl(_)
                | CrateItem::Fn(_) => None,
            })
            .collect()
    }

    fn impl_decls(&self) -> Vec<prove::ImplDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::TraitImpl(TraitImpl { binder }) => {
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
                        binder: Binder::new(
                            &vars,
                            prove::ImplDeclBoundData {
                                trait_ref: trait_id.with(self_ty, trait_parameters),
                                where_clause: where_clauses.to_wcs(),
                            },
                        ),
                    })
                }
                CrateItem::Struct(_)
                | CrateItem::Enum(_)
                | CrateItem::Trait(_)
                | CrateItem::Fn(_) => None,
            })
            .collect()
    }

    fn alias_eq_decls(&self) -> Vec<prove::AliasEqDecl> {
        self.items
            .iter()
            .flat_map(|item| match item {
                CrateItem::TraitImpl(TraitImpl { binder }) => {
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
                                (&trait_vars, &assoc_vars),
                            );

                            ensures
                                .iter()
                                .map(|e| {
                                    let fresh_var = fresh_bound_var(ParameterKind::Ty);
                                    let ensures =
                                        Binder::new(&vec![fresh_var], e.to_wc(&fresh_var));

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
    APR,
    Predicate,
    AtomicRelation,
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
            WhereClauseData::Outlives(a, b) => AtomicRelation::outlives(a, b).upcast(),
            WhereClauseData::ForAll(binder) => {
                let (vars, wc) = binder.open();
                wc.to_wcs()
                    .into_iter()
                    .map(|wc| Wc::for_all(&vars, wc))
                    .collect()
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
            WhereBoundData::Outlives(lt) => AtomicRelation::outlives(self_ty, lt).upcast(),
            WhereBoundData::ForAll(binder) => {
                let (vars, bound) = binder.open();
                Wc::for_all(&vars, bound.to_wc(self_ty))
            }
        }
    }
}
