use formality_macros::term;
use formality_types::{
    cast::Upcast,
    grammar::{
        AliasName, Binder, InferenceVar, PlaceholderVar, TraitId, TraitRef, Ty, VarIndex, Variable,
        Wc,
    },
    term::Term,
    visit::Visit,
};

#[term]
pub struct Program {
    impl_decls: Vec<ImplDecl>,
    trait_decls: Vec<TraitDecl>,
    alias_eq_decls: Vec<AliasEqDecl>,
    alias_bound_decls: Vec<AliasBoundDecl>,
}

impl Program {
    pub fn impl_decls<'s>(&'s self, trait_id: &'s TraitId) -> impl Iterator<Item = &'s ImplDecl> {
        self.impl_decls
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

    pub fn alias_eq_decls(&self) -> &[AliasEqDecl] {
        &self.alias_eq_decls
    }

    pub fn alias_bound_decls(&self) -> &[AliasBoundDecl] {
        &self.alias_bound_decls
    }

    /// Instantiates the give binder with universal placeholders that are
    /// fresh in `(context, b)`.
    pub fn instantiate_universally<C: Term, T: Term>(&self, context: &C, b: &Binder<T>) -> T {
        // Find a universe that doesn't appear in `fresh_in` or `b`.
        let universe = (context, b).max_universe().next();
        let result = b.instantiate(|kind, var_index| {
            PlaceholderVar {
                kind,
                universe,
                var_index,
            }
            .upcast()
        });
        result
    }

    /// Instantiates the give binder with existential variables that are fresh in
    /// `(context, b)` and which can name any universe appearing in `(context, b)`.
    pub fn instantiate_existentially<C: Term, T: Term>(&self, context: &C, b: &Binder<T>) -> T {
        let universe = (context, b).max_universe();
        let start = (context, b)
            .free_variables()
            .into_iter()
            .map(|v| match v {
                Variable::PlaceholderVar(_) => 0,
                Variable::InferenceVar(v) => v.var_index.index + 1,
                Variable::BoundVar(_) => 0,
            })
            .max()
            .unwrap_or(0);
        let result = b.instantiate(|kind, var_index| {
            InferenceVar {
                kind,
                universe,
                var_index: VarIndex {
                    index: var_index.index + start,
                },
            }
            .upcast()
        });
        result
    }
}

#[term(impl $binder)]
pub struct ImplDecl {
    pub binder: Binder<ImplDeclBoundData>,
}

#[term($trait_ref where $where_clause)]
pub struct ImplDeclBoundData {
    pub trait_ref: TraitRef,
    pub where_clause: Wc,
}

#[term(trait $id $binder)]
pub struct TraitDecl {
    pub id: TraitId,
    pub binder: Binder<TraitDeclBoundData>,
}

#[term(where $where_clause)]
pub struct TraitDeclBoundData {
    pub where_clause: Wc,
}

#[term(alias $id $binder)]
pub struct AliasEqDecl {
    pub id: AliasName,
    pub binder: Binder<AliasEqDeclBoundData>,
}

#[term(= $ty where $where_clause)]
pub struct AliasEqDeclBoundData {
    pub ty: Ty,
    pub where_clause: Wc,
}

#[term(alias $id $binder)]
pub struct AliasBoundDecl {
    pub id: AliasName,
    pub binder: Binder<AliasBoundDeclBoundData>,
}

#[term(: $ensures where $where_clause)]
pub struct AliasBoundDeclBoundData {
    // FIXME: this is currently encoded as something like `<T> [T: Foo]` where
    // `T` represents the alias.
    pub ensures: Binder<Wc>,
    pub where_clause: Wc,
}
