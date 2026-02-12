use crate::grammar::{
    Adt, AdtBoundData, AdtId, AliasName, AliasTy, AssociatedTyValue, AssociatedTyValueBoundData,
    Binder, Crate, CrateId, CrateItem, FieldId, ImplItem, NegTraitImpl, NegTraitImplBoundData,
    Parameter, Predicate, Program, Relation, Trait, TraitBoundData, TraitId, TraitImpl,
    TraitImplBoundData, TraitRef, Ty, VariantId, Wc, Wcs,
};
use crate::prove::ToWcs;
use formality_core::{seq, Set, To, Upcast, Upcasted};
use formality_macros::term;

#[term]
pub struct Decls {
    pub program: Program,
    pub max_size: usize,
}

impl Decls {
    /// Max size used in unit tests that are not stress testing maximum size.
    pub const DEFAULT_MAX_SIZE: usize = 222;

    pub fn is_local_trait_id(&self, trait_id: &TraitId) -> bool {
        self.program
            .crates
            .last()
            .into_iter()
            .flat_map(|c| c.items.iter())
            .any(|item| match item {
                CrateItem::Trait(t) => t.id == *trait_id,
                _ => false,
            })
    }

    pub fn is_local_adt_id(&self, adt_id: &AdtId) -> bool {
        self.program
            .crates
            .last()
            .into_iter()
            .flat_map(|c| c.items.iter())
            .any(|item| match item {
                CrateItem::Struct(s) => s.id == *adt_id,
                CrateItem::Enum(e) => e.id == *adt_id,
                _ => false,
            })
    }

    pub fn impl_decls(&self, trait_id: &TraitId) -> Vec<ImplDecl> {
        self.program
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::TraitImpl(ti) => Some(ti),
                _ => None,
            })
            .filter(|ti| ti.binder.peek().trait_id == *trait_id)
            .map(Self::grammar_trait_impl_to_decl)
            .collect()
    }

    pub fn neg_impl_decls(&self, trait_id: &TraitId) -> Vec<NegImplDecl> {
        self.program
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::NegTraitImpl(nti) => Some(nti),
                _ => None,
            })
            .filter(|nti| nti.binder.peek().trait_id == *trait_id)
            .map(Self::grammar_neg_trait_impl_to_decl)
            .collect()
    }

    /// Look up a trait by id from the program grammar and convert to a `TraitDecl`.
    pub fn trait_decl(&self, trait_id: &TraitId) -> TraitDecl {
        let grammar_trait = self.program.trait_named(trait_id).unwrap();
        Self::grammar_trait_to_decl(grammar_trait)
    }

    fn grammar_trait_impl_to_decl(ti: &TraitImpl) -> ImplDecl {
        let (
            vars,
            TraitImplBoundData {
                trait_id,
                self_ty,
                trait_parameters,
                where_clauses,
                impl_items: _,
            },
        ) = ti.binder.open();
        ImplDecl {
            safety: ti.safety.clone(),
            binder: Binder::new(
                vars,
                ImplDeclBoundData {
                    trait_ref: trait_id.with(self_ty, trait_parameters),
                    where_clause: where_clauses.to_wcs(),
                },
            ),
        }
    }

    fn grammar_neg_trait_impl_to_decl(nti: &NegTraitImpl) -> NegImplDecl {
        let (
            vars,
            NegTraitImplBoundData {
                trait_id,
                self_ty,
                trait_parameters,
                where_clauses,
            },
        ) = nti.binder.open();
        NegImplDecl {
            safety: nti.safety.clone(),
            binder: Binder::new(
                vars,
                NegImplDeclBoundData {
                    trait_ref: trait_id.with(self_ty, trait_parameters),
                    where_clause: where_clauses.to_wcs(),
                },
            ),
        }
    }

    fn grammar_trait_to_decl(t: &Trait) -> TraitDecl {
        let (
            vars,
            TraitBoundData {
                where_clauses,
                trait_items: _,
            },
        ) = t.binder.open();
        TraitDecl {
            safety: t.safety.clone(),
            id: t.id.clone(),
            binder: Binder::new(
                vars,
                TraitDeclBoundData {
                    where_clause: where_clauses.iter().flat_map(|wc| wc.to_wcs()).collect(),
                },
            ),
        }
    }

    pub fn alias_eq_decls(&self, name: &AliasName) -> Vec<AliasEqDecl> {
        self.program
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::TraitImpl(ti) => Some(ti),
                _ => None,
            })
            .flat_map(|ti| {
                let (
                    impl_vars,
                    TraitImplBoundData {
                        trait_id,
                        self_ty,
                        trait_parameters,
                        where_clauses: impl_wc,
                        impl_items,
                    },
                ) = ti.binder.open();

                impl_items
                    .iter()
                    .filter_map(|impl_item| match impl_item {
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
                            Some(AliasEqDecl {
                                binder: Binder::new(
                                    (&impl_vars, &assoc_vars),
                                    AliasEqDeclBoundData {
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
                    })
                    .collect::<Vec<_>>()
            })
            .filter(|a| a.alias_name() == *name)
            .collect()
    }

    pub fn adt_decl(&self, adt_id: &AdtId) -> AdtDecl {
        let mut v: Vec<_> = self
            .program
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::Struct(s) => Some(s.to_adt()),
                CrateItem::Enum(e) => Some(e.to_adt()),
                _ => None,
            })
            .filter(|adt| adt.id == *adt_id)
            .map(Self::grammar_adt_to_decl)
            .collect();
        assert!(!v.is_empty(), "no ADT named `{adt_id:?}`");
        assert!(v.len() <= 1, "multiple ADTs named `{adt_id:?}`");
        v.pop().unwrap()
    }

    fn grammar_adt_to_decl(adt: Adt) -> AdtDecl {
        let Adt { id, binder } = adt;
        let (
            vars,
            AdtBoundData {
                where_clauses,
                variants,
            },
        ) = binder.open();
        AdtDecl {
            id,
            binder: Binder::new(
                vars,
                AdtDeclBoundData {
                    where_clause: where_clauses.iter().flat_map(|wc| wc.to_wcs()).collect(),
                    variants: variants
                        .iter()
                        .map(|variant| variant.to_adt_decl_variant())
                        .collect(),
                },
            ),
        }
    }

    /// Return the set of "trait invariants" for all traits.
    /// See [`TraitDecl::trait_invariants`].
    pub fn trait_invariants(&self) -> Set<TraitInvariant> {
        self.program
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::Trait(t) => Some(t),
                _ => None,
            })
            .flat_map(|t| Self::grammar_trait_to_decl(t).trait_invariants())
            .collect()
    }

    /// Create a `Program` wrapping the given items in a single crate named "test".
    pub fn program_from_items(items: Vec<CrateItem>) -> Program {
        Program {
            crates: vec![Crate {
                id: CrateId::new("test"),
                items,
            }],
        }
    }

    pub fn empty() -> Self {
        Self {
            program: Program { crates: vec![] },
            max_size: Decls::DEFAULT_MAX_SIZE,
        }
    }
}

/// An "impl decl" indicates that a trait is implemented for a given set of types.
/// One "impl decl" is created for each impl in the Rust source.
#[term($?safety impl $binder)]
pub struct ImplDecl {
    /// The safety this impl declares, which needs to match the implemented trait's safety.
    pub safety: Safety,
    /// The binder covers the generic variables from the impl
    pub binder: Binder<ImplDeclBoundData>,
}

/// Data bound under the generics from [`ImplDecl`][]
#[term($trait_ref $:where $where_clause)]
pub struct ImplDeclBoundData {
    /// The trait ref that is implemented
    pub trait_ref: TraitRef,

    ///
    pub where_clause: Wcs,
}

/// A declaration that some trait will *not* be implemented for a type; derived from negative impls
/// like `impl !Foo for Bar`.
#[term($?safety impl $binder)]
pub struct NegImplDecl {
    /// The safety this negative impl declares
    pub safety: Safety,

    /// Binder comes the generics on the impl
    pub binder: Binder<NegImplDeclBoundData>,
}

/// Data bound under the impl generics for a negative impl
#[term(!$trait_ref $:where $where_clause)]
pub struct NegImplDeclBoundData {
    pub trait_ref: TraitRef,
    pub where_clause: Wcs,
}

/// Mark a trait or trait impl as `unsafe`.
#[term]
#[derive(Default)]
pub enum Safety {
    #[default]
    Safe,
    Unsafe,
}

/// A "trait declaration" declares a trait that exists, its generics, and its where-clauses.
/// It doesn't capture the trait items, which will be transformed into other sorts of rules.
///
/// In Rust syntax, it covers the `trait Foo: Bar` part of the declaration, but not what appears in the `{...}`.
#[term($?safety trait $id $binder)]
pub struct TraitDecl {
    /// The name of the trait
    pub id: TraitId,

    /// Whether the trait is `unsafe` or not
    pub safety: Safety,

    /// The binder here captures the generics of the trait; it always begins with a `Self` type.
    pub binder: Binder<TraitDeclBoundData>,
}

impl TraitDecl {
    /// Return the set of "trait invariants", i.e., things we know to be true
    /// because of the trait where-clauses. For example, given `trait Ord<ty Self> where {PartialOrd(Self)}`,
    /// this would return the set `{trait_invariant(<ty Self> Ord(Self) => PartialOrd(Self)}`
    pub fn trait_invariants(&self) -> Set<TraitInvariant> {
        let (variables, TraitDeclBoundData { where_clause }) = self.binder.open();
        let self_var: Parameter = variables[0].upcast();

        fn is_supertrait(self_var: &Parameter, wc: &Wc) -> bool {
            match wc.data() {
                crate::grammar::WcData::Predicate(Predicate::IsImplemented(trait_ref)) => {
                    trait_ref.parameters[0] == *self_var
                }
                crate::grammar::WcData::Relation(Relation::Outlives(a, _)) => *a == *self_var,
                crate::grammar::WcData::Predicate(_) => false,
                crate::grammar::WcData::Relation(_) => false,
                crate::grammar::WcData::ForAll(binder) => is_supertrait(self_var, binder.peek()),
                crate::grammar::WcData::Implies(_, c) => is_supertrait(self_var, c),
            }
        }

        where_clause
            .into_iter()
            .filter(|where_clause| is_supertrait(&self_var, where_clause))
            .map(|where_clause| TraitInvariant {
                binder: Binder::new(
                    &variables,
                    TraitInvariantBoundData {
                        trait_ref: TraitRef::new(&self.id, &variables),
                        where_clause,
                    },
                ),
            })
            .collect()
    }
}

/// A trait *invariant* is a rule like `<T> Implemented(T: Ord) => Implemented(T: PartialOrd)`.
/// It indices that, if we know that `T: Ord` from the environment,
/// we also know that `T: PartialOrd`.
/// Invariants are produced from trait declarations during lowering; they derive from the
/// where-clauses on the trait.
#[term]
pub struct TraitInvariant {
    pub binder: Binder<TraitInvariantBoundData>,
}

/// The "bound data" for a [`TraitInvariant`][] -- i.e., what is covered by the forall.
#[term($trait_ref => $where_clause)]
pub struct TraitInvariantBoundData {
    /// Knowing that this trait-ref is implemented...
    pub trait_ref: TraitRef,

    /// ...implies that these where-clauses hold.
    pub where_clause: Wc,
}

/// The "bound data" for a [`TraitDecl`][] -- i.e., what is covered by the forall.
#[term($:where $where_clause)]
pub struct TraitDeclBoundData {
    /// The where-clauses declared on the trait
    pub where_clause: Wcs,
}

/// An "alias equal declaration" declares when an alias type can be normalized
/// to something else. They are derived from `type Foo = Bar` declarations in
/// impls, which would generate an alias eq decl saying that `<T as SomeTrait>::Foo = Bar`.
#[term(alias $binder)]
pub struct AliasEqDecl {
    /// The binder includes the generics from the impl and also any generics on the GAT.
    pub binder: Binder<AliasEqDeclBoundData>,
}

impl AliasEqDecl {
    pub fn alias_name(&self) -> AliasName {
        self.binder.peek().alias.name.clone()
    }
}

/// Data bound under the impl generics for a [`AliasEqDecl`][]
#[term($alias = $ty $:where $where_clause)]
pub struct AliasEqDeclBoundData {
    /// The alias that is equal
    pub alias: AliasTy,

    /// The type the alias is equal to
    pub ty: Ty,

    /// The where-clauses that must hold for this rule to be applicable; derived from the impl and the GAT
    pub where_clause: Wcs,
}

/// An "ADT declaration" declares an ADT name, its generics, and its where-clauses, and the field.
///
/// In Rust syntax, it covers the `struct Foo<X> where X: Bar` part of the declaration, but not what appears in the `{...}`.
#[term(adt $id $binder)]
pub struct AdtDecl {
    /// The name of the ADT.
    pub id: AdtId,

    /// The binder here captures the generics of the ADT.
    pub binder: Binder<AdtDeclBoundData>,
}

/// The "bound data" for a [`AdtDecl`][].
#[term($:where $where_clause { $,variants })]
pub struct AdtDeclBoundData {
    /// The where-clauses declared on the ADT,
    pub where_clause: Wcs,
    pub variants: Vec<AdtDeclVariant>,
}

#[term($name { $,fields })]
pub struct AdtDeclVariant {
    pub name: VariantId,
    pub fields: Vec<AdtDeclField>,
}

#[term($name : $ty)]
pub struct AdtDeclField {
    pub name: AdtDeclFieldName,
    pub ty: Ty,
}

#[term]
pub enum AdtDeclFieldName {
    #[cast]
    Id(FieldId),
    #[cast]
    Index(usize),
}
