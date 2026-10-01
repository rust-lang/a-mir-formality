//! Checks specific to branch specialization
//! (`#![feature(branch_specialization)]`): the feature gates, and the impls
//! of `always_applicable` traits.

use crate::grammar::{
    expr::{Block, Stmt},
    Applicability, CrateItem, FeatureGateName, Fn, FnBody, Lt, MaySpecBound, MaybeFnBody,
    Parameter, TraitImpl, Variable, WhereClause,
};
use crate::prove::lifetimes::map_lifetimes;
use crate::prove::{Env, Program};
use formality_core::visit::CoreVisit;
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

judgment_fn! {
    /// `always_applicable trait` requires `#![feature(spec_always_applicable)]`,
    /// and every impl of such a trait must be always applicable.
    pub(crate) fn check_always_applicable(
        program: Program,
        item: CrateItem,
    ) => () {
        debug(item, program)

        (
            (if !concerns_always_applicable(&program, &item))!
            ---- ("other item")
            (check_always_applicable(program, item) => ())
        )

        (
            (if t.applicability == Applicability::Always)!
            (let enabled = program.feature_gate_enabled(&FeatureGateName::SpecAlwaysApplicable))
            (if *enabled)
            ---- ("always applicable trait")
            (check_always_applicable(program, CrateItem::Trait(t)) => ())
        )

        (
            (if program.is_always_applicable_trait(ti.trait_id()))!
            (check_always_applicable_impl(program, ti) => ())
            ---- ("impl of an always applicable trait")
            (check_always_applicable(program, CrateItem::TraitImpl(ti)) => ())
        )
    }
}

/// Is `item` an `always_applicable` trait, or an impl of one?
fn concerns_always_applicable(program: &Program, item: &CrateItem) -> bool {
    match item {
        CrateItem::Trait(t) => t.applicability == Applicability::Always,
        CrateItem::TraitImpl(ti) => program.is_always_applicable_trait(ti.trait_id()),
        _ => false,
    }
}

judgment_fn! {
    /// An impl of an `always_applicable` trait applies to a type regardless
    /// of the type's lifetimes, so that a bound on the trait can be decided
    /// after lifetime erasure (rustc's `check_always_applicable`, for impls
    /// of a `#[rustc_specialization_trait]` trait):
    ///
    /// * no `'static` in the header (`impl Foo for Tag<'static>`);
    /// * no parameter twice in the header (`impl<'a> Foo for Pair<'a, 'a>`;
    ///   also `impl<T> Foo for Pair<T, T>`, since whether two types are equal
    ///   depends on their lifetimes);
    /// * every where-clause is a bound on an `always_applicable` trait, or
    ///   mentions no parameter.
    fn check_always_applicable_impl(
        program: Program,
        ti: TraitImpl,
    ) => () {
        debug(ti)

        (
            (let (_env, data) = Env::default().instantiate_universally(&ti.binder))
            (let header: Vec<Parameter> = data.trait_ref().parameters)
            (if !mentions_static(&header))
            (let repeated = repeated_parameter(&header))
            (if repeated.is_none())
            (for_all(wc in &data.where_clauses)
                (check_always_applicable_where_clause(program, wc) => ()))
            ---- ("always applicable impl")
            (check_always_applicable_impl(program, ti) => ())
        )
    }
}

judgment_fn! {
    /// A where-clause an always applicable impl may have.
    fn check_always_applicable_where_clause(
        program: Program,
        wc: WhereClause,
    ) => () {
        debug(wc)

        (
            (let always_applicable = program.is_always_applicable_trait(&trait_id))
            (if *always_applicable)
            ---- ("bound on an always applicable trait")
            (check_always_applicable_where_clause(program, WhereClause::IsImplemented(_self_ty, trait_id, _parameters)) => ())
        )

        (
            (if wc.free_variables().is_empty())
            ---- ("closed")
            (check_always_applicable_where_clause(_program, wc) => ())
        )
    }
}

/// Does one of `parameters` mention `'static`?
fn mentions_static(parameters: &[Parameter]) -> bool {
    let mut found = false;
    for p in parameters {
        map_lifetimes(p, &mut |lt| {
            found |= *lt == Lt::Static;
            lt.clone()
        });
    }
    found
}

/// A parameter that occurs twice in `parameters`, if any.
fn repeated_parameter(parameters: &[Parameter]) -> Option<Variable> {
    let mut vars = parameters.free_variables();
    vars.sort();
    vars.windows(2).find(|w| w[0] == w[1]).map(|w| w[0])
}
