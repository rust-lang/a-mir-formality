//! Checks specific to branch specialization
//! (`#![feature(branch_specialization)]`): the feature gate.

use crate::grammar::{
    expr::{Block, Stmt},
    CrateItem, FeatureGateName, Fn, FnBody, MaySpecBound, MaybeFnBody, WhereClause,
};
use crate::prove::Program;
use formality_core::{judgment_fn, Downcasted};

/// True if `#![feature(branch_specialization)]` is enabled anywhere in the
/// program.
pub(crate) fn branch_specialization_enabled(program: &Program) -> bool {
    program.feature_gate_enabled(&FeatureGateName::BranchSpecialization)
}

/// The bounds of the `may_spec` where-clauses among `where_clauses`.
fn may_spec_bounds(where_clauses: &[WhereClause]) -> Vec<MaySpecBound> {
    where_clauses
        .iter()
        .filter_map(|wc| match wc {
            WhereClause::MaySpec(bound) => Some(bound.clone()),
            _ => None,
        })
        .collect()
}

/// The conditions of the `if impls` statements in `block`.
fn if_impls_conditions(block: &Block) -> Vec<MaySpecBound> {
    block
        .stmts
        .iter()
        .flat_map(stmt_if_impls_conditions)
        .collect()
}

fn stmt_if_impls_conditions(stmt: &Stmt) -> Vec<MaySpecBound> {
    match stmt {
        Stmt::IfImpls {
            condition,
            then_block,
            else_block,
        } => std::iter::once(condition.clone())
            .chain(if_impls_conditions(then_block))
            .chain(if_impls_conditions(&else_block.block))
            .collect(),
        Stmt::If {
            then_block,
            else_block,
            ..
        } => if_impls_conditions(then_block)
            .into_iter()
            .chain(if_impls_conditions(&else_block.block))
            .collect(),
        Stmt::Loop { body, .. } => if_impls_conditions(body),
        Stmt::Block(block) => if_impls_conditions(block),
        Stmt::Exists { binder } => if_impls_conditions(binder.peek()),
        Stmt::Let { .. }
        | Stmt::Expr { .. }
        | Stmt::Break { .. }
        | Stmt::Continue { .. }
        | Stmt::Return { .. }
        | Stmt::Print { .. } => vec![],
    }
}

/// The bounds `f` uses branch specialization on: its `may_spec` bounds and
/// the conditions of the `if impls` in its body.
fn fn_specialization_bounds(f: &Fn) -> Vec<MaySpecBound> {
    let bound = f.binder.peek();
    let mut bounds = may_spec_bounds(&bound.where_clauses);
    if let MaybeFnBody::FnBody(FnBody::Expr(block)) = &bound.body {
        bounds.extend(if_impls_conditions(block));
    }
    bounds
}

/// The bounds `item` uses branch specialization on.
fn item_specialization_bounds(item: &CrateItem) -> Vec<MaySpecBound> {
    match item {
        CrateItem::Fn(f) => fn_specialization_bounds(f),
        CrateItem::TraitImpl(ti) => {
            let bound = ti.binder.peek();
            let mut bounds = may_spec_bounds(&bound.where_clauses);
            for f in bound.impl_items.iter().downcasted::<Fn>() {
                bounds.extend(fn_specialization_bounds(&f));
            }
            bounds
        }
        CrateItem::Trait(t) => {
            let bound = t.binder.explicit_binder.peek();
            let mut bounds = may_spec_bounds(&bound.where_clauses);
            for f in bound.trait_items.iter().downcasted::<Fn>() {
                bounds.extend(fn_specialization_bounds(&f));
            }
            bounds
        }
        CrateItem::AdtItem(adt) => may_spec_bounds(&adt.where_clauses()),
        CrateItem::NegTraitImpl(nti) => may_spec_bounds(&nti.binder.peek().where_clauses),
        CrateItem::FeatureGate(_) | CrateItem::Test(_) => vec![],
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
            (if item_specialization_bounds(&item).is_empty())!
            ---- ("no branch specialization")
            (check_branch_specialization(_program, item) => ())
        )

        (
            (if !item_specialization_bounds(&item).is_empty())!
            (let enabled = branch_specialization_enabled(&program))
            (if *enabled)
            ---- ("feature gate")
            (check_branch_specialization(program, item) => ())
        )
    }
}
