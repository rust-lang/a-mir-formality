use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{
        AdtId, AliasName, AliasTy, Binder, Parameter, ParameterData, Predicate, Relation,
        RigidName, TraitId, TraitRef, Ty, TyData, Variable, Wc, Wcs, PR,
    },
    set,
};

#[term]
pub struct Decls {
    pub max_size: usize,
    pub trait_decls: Vec<TraitDecl>,
    pub impl_decls: Vec<ImplDecl>,
    pub neg_impl_decls: Vec<NegImplDecl>,
    pub alias_eq_decls: Vec<AliasEqDecl>,
    pub alias_bound_decls: Vec<AliasBoundDecl>,
    pub local_trait_ids: Set<TraitId>,
    pub local_adt_ids: Set<AdtId>,
}

impl Decls {
    /// Max size used in unit tests that are not stress testing maximum size.
    pub const DEFAULT_MAX_SIZE: usize = 222;

    pub fn is_local_trait_ref(&self, trait_ref: &TraitRef) -> bool {
        // From https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html:
        //
        // Given `impl<P1..=Pn> Trait<T1..=Tn> for T0`, an impl is valid only if at least one of the following is true:
        //
        // - `Trait` is a local trait
        // - All of
        //  - At least one of the types `T0..=Tn` must be a local type. Let `Ti` be the
        //    first such type.
        //  - No uncovered type parameters `P1..=Pn` may appear in `T0..Ti` (excluding
        //    `Ti`)
        //
        // Given the following definitions:
        //
        // Covered Type: A type which appears as a parameter to another type. For example,
        // `T` is uncovered, but the `T` in `Vec<T>` is covered. This is only relevant for
        // type parameters.
        //
        // Fundamental Type: A type for which you cannot add a blanket impl backwards
        // compatibly. This includes `&`, `&mut`, and `Box`. Any time a type `T` is
        // considered local, `&T`, `&mut T`, and `Box<T>` are also considered local.
        // Fundamental types cannot cover other types. Any time the term "covered type" is
        // used, `&T`, `&mut T`, and `Box<T>` are not considered covered.
        //
        // Local Type: A struct, enum, or union which was defined in the current crate.
        // This is not affected by type parameters. `struct Foo` is considered local, but
        // `Vec<Foo>` is not. `LocalType<ForeignType>` is local. Type aliases and trait
        // aliases do not affect locality.
        self.is_local_trait_id(&trait_ref.trait_id) || {
            if let Some(first_local) = trait_ref
                .parameters
                .iter()
                .position(|p| self.is_local_parameter(p))
            {
                let parameters_before = &trait_ref.parameters[..first_local];
                !parameters_before
                    .iter()
                    .any(|p| self.may_have_downstream_type(p))
            } else {
                false
            }
        }
    }

    pub fn is_local_parameter(&self, parameter: impl Upcast<Parameter>) -> bool {
        let parameter: Parameter = parameter.upcast();
        match parameter.data() {
            ParameterData::Ty(t) => match t {
                TyData::RigidTy(r) => {
                    if self.is_fundamental(&r.name) {
                        r.parameters.iter().all(|p| self.is_local_parameter(p))
                    } else {
                        match &r.name {
                            RigidName::AdtId(a) => self.is_local_adt_id(a),
                            RigidName::ScalarId(_) => false,
                            RigidName::Ref(_) => unreachable!("refs are fundamental"),
                            RigidName::Tuple(_) => todo!(),
                            RigidName::FnPtr(_) => false,
                            RigidName::FnDef(_) => todo!(),
                        }
                    }
                }
                TyData::AliasTy(_) => todo!(),
                TyData::PredicateTy(_) => todo!(),
                TyData::Variable(v) => match v {
                    Variable::PlaceholderVar(_) | Variable::InferenceVar(_) => false,
                    Variable::BoundVar(_) => todo!(),
                },
            },
            ParameterData::Lt(_) => false,
        }
    }

    pub fn is_local_trait_id(&self, trait_id: &TraitId) -> bool {
        self.local_trait_ids.contains(trait_id)
    }

    pub fn is_local_adt_id(&self, adt_id: &AdtId) -> bool {
        self.local_adt_ids.contains(adt_id)
    }

    pub fn is_fundamental(&self, name: &RigidName) -> bool {
        // Fundamental Type: A type for which you cannot add a blanket impl backwards
        // compatibly. This includes `&`, `&mut`, and `Box`. Any time a type `T` is
        // considered local, `&T`, `&mut T`, and `Box<T>` are also considered local.
        // Fundamental types cannot cover other types. Any time the term "covered type" is
        // used, `&T`, `&mut T`, and `Box<T>` are not considered covered.

        match name {
            RigidName::AdtId(_) => false, // FIXME

            RigidName::Ref(_) => true,

            RigidName::ScalarId(_)
            | RigidName::Tuple(_)
            | RigidName::FnPtr(_)
            | RigidName::FnDef(_) => false,
        }
    }

    pub fn impl_decls<'s>(&'s self, trait_id: &'s TraitId) -> impl Iterator<Item = &'s ImplDecl> {
        self.impl_decls
            .iter()
            .filter(move |i| i.binder.peek().trait_ref.trait_id == *trait_id)
    }

    pub fn neg_impl_decls<'s>(
        &'s self,
        trait_id: &'s TraitId,
    ) -> impl Iterator<Item = &'s NegImplDecl> {
        self.neg_impl_decls
            .iter()
            .filter(move |i| i.binder.peek().trait_ref.trait_id == *trait_id)
    }

    pub fn trait_decl(&self, trait_id: &TraitId) -> &TraitDecl {
        let mut v: Vec<_> = self
            .trait_decls
            .iter()
            .filter(|t| t.id == *trait_id)
            .collect();
        assert!(v.len() > 0, "no traits named `{trait_id:?}`");
        assert!(v.len() <= 1, "multiple traits named `{trait_id:?}`");
        v.pop().unwrap()
    }

    pub fn alias_eq_decls<'s>(
        &'s self,
        name: &'s AliasName,
    ) -> impl Iterator<Item = &'s AliasEqDecl> {
        self.alias_eq_decls
            .iter()
            .filter(move |a| a.alias_name() == *name)
    }

    pub fn alias_bound_decls(&self) -> &[AliasBoundDecl] {
        &self.alias_bound_decls
    }

    /// Return the set of "trait invariants" for all traits.
    /// See [`TraitDecl::trait_invariants`].
    pub fn trait_invariants(&self) -> Set<TraitInvariant> {
        self.trait_decls
            .iter()
            .flat_map(|td| td.trait_invariants())
            .collect()
    }

    pub fn empty() -> Self {
        Self {
            max_size: Decls::DEFAULT_MAX_SIZE,
            trait_decls: vec![],
            impl_decls: vec![],
            neg_impl_decls: vec![],
            alias_eq_decls: vec![],
            alias_bound_decls: vec![],
            local_trait_ids: set![],
            local_adt_ids: set![],
        }
    }
}

#[term(impl $binder)]
pub struct ImplDecl {
    pub binder: Binder<ImplDeclBoundData>,
}

#[term($trait_ref where $where_clause)]
pub struct ImplDeclBoundData {
    pub trait_ref: TraitRef,
    pub where_clause: Wcs,
}

#[term(impl $binder)]
pub struct NegImplDecl {
    pub binder: Binder<NegImplDeclBoundData>,
}

#[term(!$trait_ref where $where_clause)]
pub struct NegImplDeclBoundData {
    pub trait_ref: TraitRef,
    pub where_clause: Wcs,
}

#[term(trait $id $binder)]
pub struct TraitDecl {
    pub id: TraitId,
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
                formality_types::grammar::WcData::PR(PR::Predicate(Predicate::IsImplemented(
                    trait_ref,
                ))) => trait_ref.parameters[0] == *self_var,
                formality_types::grammar::WcData::PR(PR::Relation(Relation::Outlives(a, _))) => {
                    *a == *self_var
                }
                formality_types::grammar::WcData::PR(_) => false,
                formality_types::grammar::WcData::ForAll(binder) => {
                    is_supertrait(self_var, binder.peek())
                }
                formality_types::grammar::WcData::Implies(_, c) => is_supertrait(self_var, c),
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

#[term]
pub struct TraitInvariant {
    pub binder: Binder<TraitInvariantBoundData>,
}

#[term($trait_ref => $where_clause)]
pub struct TraitInvariantBoundData {
    pub trait_ref: TraitRef,
    pub where_clause: Wc,
}

#[term(where $where_clause)]
pub struct TraitDeclBoundData {
    pub where_clause: Wcs,
}

#[term(alias $binder)]
pub struct AliasEqDecl {
    pub binder: Binder<AliasEqDeclBoundData>,
}

impl AliasEqDecl {
    pub fn alias_name(&self) -> AliasName {
        self.binder.peek().alias.name.clone()
    }
}

#[term($alias = $ty where $where_clause)]
pub struct AliasEqDeclBoundData {
    pub alias: AliasTy,
    pub ty: Ty,
    pub where_clause: Wcs,
}

#[term(alias $binder)]
pub struct AliasBoundDecl {
    pub binder: Binder<AliasBoundDeclBoundData>,
}

impl AliasBoundDecl {
    pub fn alias_name(&self) -> AliasName {
        self.binder.peek().alias.name.clone()
    }
}

#[term($alias : $ensures where $where_clause)]
pub struct AliasBoundDeclBoundData {
    pub alias: AliasTy,
    // FIXME: this is currently encoded as something like `<T> [T: Foo]` where
    // `T` represents the alias.
    pub ensures: Binder<Wc>,
    pub where_clause: Wcs,
}
