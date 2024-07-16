use formality_core::{
    fuzz::{FuzzCx, Fuzzable},
    Upcast,
};

use crate::{
    fuzz::RustTypesFuzzConfig,
    grammar::{AssociatedItemId, Const, TraitId},
    rust::FormalityLang,
};

use super::{AssociatedTyName, Lt, ParameterKind, Parameters, RigidName, RigidTy, Ty};

impl Fuzzable<FormalityLang> for RigidTy {
    fn estimate_cardinality(cx: &mut FuzzCx<'_, FormalityLang>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
            guard.estimate_cardinality::<RigidName>() * guard.estimate_cardinality::<Parameters>()
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, FormalityLang>) -> Option<Self> {
        cx.enter_fuzz(|guard| {
            let name = guard.fuzz::<RigidName>()?;

            // Find the right kinds for that name
            let parameter_kinds = match &name {
                RigidName::AdtId(adt_id) => guard.adt_kinds(adt_id),
                RigidName::ScalarId(_) => vec![],
                RigidName::Ref(_) => vec![ParameterKind::Lt, ParameterKind::Ty],
                RigidName::Tuple(arity) => vec![ParameterKind::Ty; *arity],
                RigidName::FnPtr(_) => return None, // FIXME
                RigidName::FnDef(_) => return None, // FIXME
            };

            // Generate parameters of the correct kinds
            let parameters = parameter_kinds
                .iter()
                .map(|k| match k {
                    ParameterKind::Ty => guard.fuzz::<Ty>().upcast(),
                    ParameterKind::Lt => guard.fuzz::<Lt>().upcast(),
                    ParameterKind::Const => guard.fuzz::<Const>().upcast(),
                })
                .collect::<Option<_>>()?;

            Some(RigidTy { name, parameters })
        })
    }
}

impl Fuzzable<FormalityLang> for AssociatedTyName {
    fn estimate_cardinality(cx: &mut FuzzCx<'_, FormalityLang>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
            guard.estimate_cardinality::<AssociatedItemId>()
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, FormalityLang>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let item_id: AssociatedItemId = guard.fuzz()?;
            let (trait_id, parameter_kinds): (TraitId, Vec<ParameterKind>) =
                guard.key_value(&item_id)?;
            Some(AssociatedTyName {
                trait_id,
                item_id,
                item_arity: parameter_kinds.len(),
            })
        })
    }
}
