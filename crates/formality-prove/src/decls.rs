use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    derive_links::{DowncastTo, UpcastFrom},
    fold::Fold,
    grammar::{
        AdtId, AliasName, AliasTy, Binder, Parameter, Predicate, Relation, TraitId, TraitRef, Ty,
        Wc, Wcs, PR,
    },
    parse::{self, Parse},
    set,
    term::Term,
    visit::Visit,
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

    pub fn is_local_trait_id(&self, trait_id: &TraitId) -> bool {
        self.local_trait_ids.contains(trait_id)
    }

    pub fn is_local_adt_id(&self, adt_id: &AdtId) -> bool {
        self.local_adt_ids.contains(adt_id)
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

#[term($safety impl $binder)]
pub struct ImplDecl {
    pub safety: Safety,
    pub binder: Binder<ImplDeclBoundData>,
}

#[term($trait_ref where $where_clause)]
pub struct ImplDeclBoundData {
    pub trait_ref: TraitRef,
    pub where_clause: Wcs,
}

#[term($safety impl $binder)]
pub struct NegImplDecl {
    pub safety: Safety,
    pub binder: Binder<NegImplDeclBoundData>,
}

#[term(!$trait_ref where $where_clause)]
pub struct NegImplDeclBoundData {
    pub trait_ref: TraitRef,
    pub where_clause: Wcs,
}

/// Mark a trait or trait impl as `unsafe`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Safety {
    Safe,
    Unsafe,
}

// NOTE(yosh): `Debug` is currently used to print error messages with. In order
// to not print `safe impl` / `safe trait` where none is written, we leave the impl blank.
impl std::fmt::Debug for Safety {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Safe => write!(f, ""),
            Self::Unsafe => write!(f, "unsafe"),
        }
    }
}

impl Term for Safety {}

impl DowncastTo<Self> for Safety {
    fn downcast_to(&self) -> Option<Self> {
        Some(Self::clone(self))
    }
}

impl UpcastFrom<Self> for Safety {
    fn upcast_from(term: Self) -> Self {
        term
    }
}

impl Fold for Safety {
    fn substitute(&self, _substitution_fn: formality_types::fold::SubstitutionFn<'_>) -> Self {
        self.clone()
    }
}

impl Visit for Safety {
    fn free_variables(&self) -> Vec<formality_types::grammar::Variable> {
        vec![]
    }

    fn size(&self) -> usize {
        1
    }

    fn assert_valid(&self) {}
}

impl Parse for Safety {
    fn parse<'t>(
        _scope: &formality_types::parse::Scope,
        text0: &'t str,
    ) -> formality_types::parse::ParseResult<'t, Self> {
        match parse::expect_optional_keyword("unsafe", text0) {
            Some(text1) => Ok((Self::Unsafe, text1)),
            None => Ok((Self::Safe, text0)),
        }
    }
}

#[term($safety trait $id $binder)]
pub struct TraitDecl {
    pub safety: Safety,
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
