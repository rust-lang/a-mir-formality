use formality_core::{set, Set, Upcast};
use formality_macros::term;
use formality_types::grammar::{
    AdtId, AliasName, AliasTy, Binder, FnId, Parameter, Predicate, Relation, TraitId, TraitRef, Ty,
    Wc, Wcs,
};

#[term]
pub struct Decls {
    pub max_size: usize,

    /// Each trait in the program
    pub trait_decls: Vec<TraitDecl>,
    pub impl_decls: Vec<ImplDecl>,
    pub neg_impl_decls: Vec<NegImplDecl>,
    pub alias_eq_decls: Vec<AliasEqDecl>,
    pub alias_bound_decls: Vec<AliasBoundDecl>,
    pub adt_decls: Vec<AdtDecl>,
    pub fn_decls: Vec<FnDecl>,
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
        assert!(!v.is_empty(), "no traits named `{trait_id:?}`");
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

    pub fn adt_decl(&self, adt_id: &AdtId) -> &AdtDecl {
        let mut v: Vec<_> = self.adt_decls.iter().filter(|t| t.id == *adt_id).collect();
        assert!(!v.is_empty(), "no ADT named `{adt_id:?}`");
        assert!(v.len() <= 1, "multiple ADTs named `{adt_id:?}`");
        v.pop().unwrap()
    }

    pub fn fn_decl(&self, fn_id: &FnId) -> &FnDecl {
        let mut v: Vec<_> = self.fn_decls.iter().filter(|t| t.id == *fn_id).collect();
        assert!(!v.is_empty(), "no fn named `{fn_id:?}`");
        assert!(v.len() <= 1, "multiple fns named `{fn_id:?}`");
        v.pop().unwrap()
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
            adt_decls: vec![],
            fn_decls: vec![],
            local_trait_ids: set![],
            local_adt_ids: set![],
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
                formality_types::grammar::WcData::Predicate(Predicate::IsImplemented(
                    trait_ref,
                )) => trait_ref.parameters[0] == *self_var,
                formality_types::grammar::WcData::Relation(Relation::Outlives(a, _)) => {
                    *a == *self_var
                }
                formality_types::grammar::WcData::Predicate(_) => false,
                formality_types::grammar::WcData::Relation(_) => false,
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

/// Alias bounds indicate things that are always known to be true of an alias type,
/// even when its precise value is not known.
/// For example given a trait `trait Foo { type Bar: Baz; }`
/// we know that `<T as Foo>::Bar: Baz` must hold.
#[term(alias $binder)]
pub struct AliasBoundDecl {
    pub binder: Binder<AliasBoundDeclBoundData>,
}

impl AliasBoundDecl {
    pub fn alias_name(&self) -> AliasName {
        self.binder.peek().alias.name.clone()
    }
}

#[term($alias : $ensures $:where $where_clause)]
pub struct AliasBoundDeclBoundData {
    pub alias: AliasTy,
    // FIXME: this is currently encoded as something like `<T> [T: Foo]` where
    // `T` represents the alias.
    pub ensures: Binder<Wc>,
    pub where_clause: Wcs,
}

/// An "ADT declaration" declares an ADT name, its generics, and its where-clauses.
/// It doesn't capture the ADT fields, yet.
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
#[term($:where $where_clause)]
pub struct AdtDeclBoundData {
    /// The where-clauses declared on the ADT,
    pub where_clause: Wcs,
}

/// A "function declaration" declares a function name, its generics, its input and ouput types, and its where-clauses.
/// It doesn't currently capture the function body, or input argument names.
///
/// In Rust syntax, it covers the `fn foo<T, U>(_: T) -> U where T: Bar`
#[term(fn $id $binder)]
pub struct FnDecl {
    pub id: FnId,
    pub binder: Binder<FnDeclBoundData>,
}

/// The "bound data" for a [`FnDecl`][].
#[term(($input_tys) -> $output_ty $:where $where_clause)]
pub struct FnDeclBoundData {
    pub input_tys: Vec<Ty>,
    pub output_ty: Ty,
    pub where_clause: Wcs,
}
