//! Checks specific to branch specialization
//! (`#![feature(branch_specialization)]`): the feature gate.

use crate::grammar::{
    expr::{Block, Stmt},
    CrateItem, FeatureGateName, Fn, FnBody, ImplItem, MaybeFnBody, TraitItem, WhereClause,
};
use crate::prove::Program;
use formality_core::judgment_fn;

/// True if `#![feature(branch_specialization)]` is enabled anywhere in the
/// program.
pub(crate) fn branch_specialization_enabled(program: &Program) -> bool {
    program.feature_gate_enabled(&FeatureGateName::BranchSpecialization)
}

/// Does one of `where_clauses` use `may_spec`, under a `for<..>` included?
fn has_may_spec(where_clauses: &[WhereClause]) -> bool {
    where_clauses.iter().any(|wc| match wc {
        WhereClause::MaySpec(..) => true,
        WhereClause::ForAll(binder) => has_may_spec(std::slice::from_ref(binder.peek())),
        _ => false,
    })
}

/// Does `block` contain an `if impls`?
fn has_if_impls(block: &Block) -> bool {
    block.stmts.iter().any(stmt_has_if_impls)
}

fn stmt_has_if_impls(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::IfImpls { .. } => true,
        Stmt::If {
            then_block,
            else_block,
            ..
        } => has_if_impls(then_block) || has_if_impls(&else_block.block),
        Stmt::Loop { body, .. } => has_if_impls(body),
        Stmt::Block(block) => has_if_impls(block),
        Stmt::Exists { binder } => has_if_impls(binder.peek()),
        Stmt::Let { .. }
        | Stmt::Expr { .. }
        | Stmt::Break { .. }
        | Stmt::Continue { .. }
        | Stmt::Return { .. }
        | Stmt::Print { .. } => false,
    }
}

/// Does `f` use branch specialization: a `may_spec` bound, or an `if impls`
/// in its body?
fn fn_uses_branch_specialization(f: &Fn) -> bool {
    let bound = f.binder.peek();
    has_may_spec(&bound.where_clauses)
        || matches!(&bound.body, MaybeFnBody::FnBody(FnBody::Expr(block)) if has_if_impls(block))
}

/// Does `item` use branch specialization?
fn item_uses_branch_specialization(item: &CrateItem) -> bool {
    match item {
        CrateItem::Fn(f) => fn_uses_branch_specialization(f),
        CrateItem::TraitImpl(ti) => {
            let bound = ti.binder.peek();
            has_may_spec(&bound.where_clauses)
                || bound.impl_items.iter().any(|item| match item {
                    ImplItem::Fn(f) => fn_uses_branch_specialization(f),
                    ImplItem::AssociatedTyValue(v) => has_may_spec(&v.binder.peek().where_clauses),
                })
        }
        CrateItem::Trait(t) => {
            let bound = t.binder.explicit_binder.peek();
            has_may_spec(&bound.where_clauses)
                || bound.trait_items.iter().any(|item| match item {
                    TraitItem::Fn(f) => fn_uses_branch_specialization(f),
                    TraitItem::AssociatedTy(ty) => has_may_spec(&ty.binder.peek().where_clauses),
                })
        }
        CrateItem::AdtItem(adt) => has_may_spec(&adt.where_clauses()),
        CrateItem::NegTraitImpl(nti) => has_may_spec(&nti.binder.peek().where_clauses),
        CrateItem::FeatureGate(_) | CrateItem::Test(_) => false,
    }
}

judgment_fn! {
    /// `may_spec` bounds and `if impls` require the feature gate.
    pub(crate) fn check_branch_specialization(
        program: Program,
        item: CrateItem,
    ) => () {
        debug(item, program)

        (
            (if !item_uses_branch_specialization(&item))!
            ---- ("no branch specialization")
            (check_branch_specialization(_program, item) => ())
        )

        (
            (if item_uses_branch_specialization(&item))!
            (let branch_specialization_enabled = branch_specialization_enabled(&program))
            (if *branch_specialization_enabled)
            ---- ("feature gate")
            (check_branch_specialization(program, item) => ())
        )
    }
}
