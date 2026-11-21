use crate::grammar::{
    AliasName, Const, ImplItem, Lt, Parameter, ParameterKind, PredicateTy, TraitRef, Ty, Variable,
    WhereClause,
};
use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, Set};

judgment_fn! {
    /// An impl's type and const parameters must be determined by its trait ref.
    /// Lifetimes need this check only when used in an associated type value.
    pub(super) fn check_impl_parameters(
        parameters: Vec<Parameter>,
        trait_ref: TraitRef,
        where_clauses: Vec<WhereClause>,
        impl_items: Vec<ImplItem>,
    ) => () {
        debug(parameters, trait_ref, where_clauses)

        (
            (let constrained = constrained_parameters(trait_ref, where_clauses))
            (let required = required_parameters(parameters, impl_items))
            (for_all(parameter in required)
                (if constrained.contains(parameter)))
            ---- ("impl parameters are constrained")
            (check_impl_parameters(parameters, trait_ref, where_clauses, impl_items) => ())
        )
    }
}

fn required_parameters(parameters: &[Parameter], impl_items: &[ImplItem]) -> Set<Variable> {
    let used_in_associated_types: Set<_> = impl_items
        .iter()
        .filter_map(|item| match item {
            ImplItem::AssociatedTyValue(value) => Some(value.binder.map(|data| data.ty)),
            ImplItem::Fn(_) => None,
        })
        .flat_map(|ty| ty.free_variables())
        .collect();

    parameters
        .iter()
        .map(|parameter| parameter.as_variable().expect("impl binder parameter"))
        .filter(|variable| {
            variable.kind() != ParameterKind::Lt || used_in_associated_types.contains(variable)
        })
        .collect()
}

fn constrained_parameters(trait_ref: &TraitRef, where_clauses: &[WhereClause]) -> Set<Variable> {
    let mut constrained: Set<_> = trait_ref
        .parameters
        .iter()
        .flat_map(parameters_in)
        .collect();

    // An associated type equality can determine more parameters once its inputs
    // are known. Repeat because the bounds may be written in any order.
    loop {
        let previous_len = constrained.len();
        for clause in where_clauses {
            constrained.extend(parameters_from_equality(trait_ref, clause, &constrained));
        }
        if constrained.len() == previous_len {
            return constrained;
        }
    }
}

fn parameters_from_equality(
    trait_ref: &TraitRef,
    clause: &WhereClause,
    constrained: &Set<Variable>,
) -> Set<Variable> {
    match clause {
        WhereClause::AliasEq(alias, ty) => {
            let AliasName::AssociatedTyId(name) = &alias.name;
            let trait_parameters = &alias.parameters[..alias.parameters.len() - name.item_arity];
            // The impl cannot use its own associated type to justify itself.
            if name.trait_id == trait_ref.trait_id && trait_parameters == trait_ref.parameters {
                return Set::new();
            }
            if alias
                .parameters
                .free_variables()
                .iter()
                .all(|variable| constrained.contains(variable))
            {
                parameters_in_ty(ty)
            } else {
                Set::new()
            }
        }
        WhereClause::ForAll(binder) => {
            parameters_from_equality(trait_ref, binder.peek(), constrained)
        }
        _ => Set::new(),
    }
}

/// Collect parameters that can be recovered from the structure of a value/type.
/// An associated type does not determine its inputs: many types can have the
/// same associated type. A const expression need not determine its inputs either.
fn parameters_in(parameter: &Parameter) -> Set<Variable> {
    match parameter {
        Parameter::Ty(ty) => parameters_in_ty(ty),
        Parameter::Lt(lt) => match &**lt {
            Lt::Variable(variable) => free_variable(*variable),
            Lt::Static | Lt::Erased => Set::new(),
        },
        Parameter::Const(constant) => parameters_in_const(constant),
    }
}

fn parameters_in_ty(ty: &Ty) -> Set<Variable> {
    match ty {
        Ty::Variable(variable) => free_variable(*variable),
        Ty::RigidTy(ty) => ty.parameters.iter().flat_map(parameters_in).collect(),
        Ty::PredicateTy(PredicateTy::ForAll(binder)) => parameters_in_ty(binder.peek()),
        Ty::AliasTy(_) => Set::new(),
    }
}

fn parameters_in_const(constant: &Const) -> Set<Variable> {
    match constant {
        Const::Variable(variable) => free_variable(*variable),
        Const::RigidValue(value) => value
            .parameters
            .iter()
            .flat_map(parameters_in)
            .chain(value.values.iter().flat_map(parameters_in_const))
            .collect(),
        Const::Scalar(_) | Const::Block(_) => Set::new(),
    }
}

fn free_variable(variable: Variable) -> Set<Variable> {
    // A nested binder's local parameters are not parameters of the impl.
    std::iter::once(variable)
        .filter(Variable::is_free)
        .collect()
}
