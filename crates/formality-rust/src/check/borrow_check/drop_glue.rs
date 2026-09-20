//! Whether dropping a value of a given type can run user-written code.

use crate::grammar::{Adt, AdtBoundData, AdtId, Parameter, RigidName, RigidTy, Ty, Variant};
use crate::prove::Program;
use formality_core::Set;

/// True if dropping a value of type `ty` runs any user-written `Drop::drop`.
///
/// Like rustc's `Ty::needs_drop`, this is conservative: a type we cannot see
/// through is assumed to have glue.
pub fn ty_needs_drop(program: &Program, ty: &Ty) -> bool {
    ty_needs_drop_inner(program, ty, &mut Set::default())
}

fn ty_needs_drop_inner(program: &Program, ty: &Ty, visiting: &mut Set<AdtId>) -> bool {
    match ty {
        Ty::RigidTy(RigidTy { name, parameters }) => match name {
            RigidName::AdtId(adt_id) => adt_needs_drop(program, adt_id, parameters, visiting),

            RigidName::Tuple(_) => parameters
                .iter()
                .any(|p| parameter_needs_drop(program, p, visiting)),

            RigidName::ScalarId(_)
            | RigidName::Ref(_)
            | RigidName::Raw(_)
            | RigidName::FnPtr(_)
            | RigidName::FnDef(_)
            | RigidName::Never => false,
        },

        Ty::AliasTy(_) | Ty::PredicateTy(_) | Ty::Variable(_) => true,
    }
}

fn parameter_needs_drop(program: &Program, p: &Parameter, visiting: &mut Set<AdtId>) -> bool {
    match p {
        Parameter::Ty(ty) => ty_needs_drop_inner(program, ty, visiting),
        Parameter::Lt(_) | Parameter::Const(_) => false,
    }
}

fn adt_needs_drop(
    program: &Program,
    adt_id: &AdtId,
    parameters: &[Parameter],
    visiting: &mut Set<AdtId>,
) -> bool {
    // Rust requires a Drop impl's where-clauses to match the ADT's, so any
    // instantiation has glue.
    if program.find_drop_impl(adt_id).is_some() {
        return true;
    }

    // A well-formed type is finite, so re-entry means the type is uninhabited.
    if !visiting.insert(adt_id.clone()) {
        return false;
    }

    // Otherwise glue is inherited from the fields, with generics substituted:
    // `Wrapper<PrintOnDrop>` has glue, `Wrapper<u32>` does not.
    let result = (|| {
        let Ok(adt_item) = program.crates.adt_item_named(adt_id) else {
            return true; // unknown ADT: assume the worst
        };
        let Adt { id: _, binder } = adt_item.to_adt();
        let Ok(AdtBoundData {
            where_clauses: _,
            variants,
        }) = binder.instantiate_with(parameters)
        else {
            return true;
        };
        variants.iter().any(|Variant { name: _, fields }| {
            fields
                .iter()
                .any(|f| ty_needs_drop_inner(program, &f.ty, visiting))
        })
    })();

    visiting.remove(adt_id);
    result
}
