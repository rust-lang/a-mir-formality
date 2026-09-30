//! Checks specific to branch specialization
//! (`#![feature(branch_specialization)]`): the feature gates, and the impls
//! of `always_applicable` traits.

use crate::grammar::{
    expr::{Block, Stmt},
    Applicability, CrateItem, FeatureGateName, Fn, FnBody, ImplItem, Lt, MaybeFnBody, Parameter,
    TraitImpl, TraitItem, Variable, WhereClause,
};
use crate::prove::lifetimes::map_lifetimes;
use crate::prove::{enter_universally, Env, Program};
use formality_core::judgment_fn;
use formality_core::visit::CoreVisit;

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
            (let spec_always_applicable_enabled = program.feature_gate_enabled(&FeatureGateName::SpecAlwaysApplicable))
            (if *spec_always_applicable_enabled)
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
            (scope(enter_universally(Env::default(), &ti.binder) => (_env, data)) with()
                (let header: Vec<Parameter> = data.trait_ref().parameters)
                (if !mentions_static(&header))
                (let repeated = repeated_parameter(&header))
                (if repeated.is_none())
                (for_all(wc in &data.where_clauses)
                    (check_always_applicable_where_clause(program, wc) => ())))
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
