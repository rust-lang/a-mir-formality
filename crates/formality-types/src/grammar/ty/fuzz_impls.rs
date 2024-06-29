use formality_core::Upcast;

use crate::{
    fuzz::FuzzCx,
    grammar::{AssociatedItemId, Const},
};

use super::{AssociatedTyName, Lt, ParameterKind, RigidName, RigidTy, Ty};

impl bolero::TypeGenerator for RigidTy {
    fn generate<D: bolero::Driver>(driver: &mut D) -> Option<Self> {
        // This is customized so that we can be sure to generate types
        // with property arity.

        // First create the name
        let name: RigidName = driver.gen()?;

        // Find the right kinds for that name
        let parameter_kinds = match &name {
            RigidName::AdtId(adt_id) => FuzzCx::adt_id_map().get().get(adt_id)?.clone(),
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
                ParameterKind::Ty => driver.gen::<Ty>().upcast(),
                ParameterKind::Lt => driver.gen::<Lt>().upcast(),
                ParameterKind::Const => driver.gen::<Const>().upcast(),
            })
            .collect::<Option<_>>()?;

        Some(RigidTy { name, parameters })
    }
}

impl bolero::TypeGenerator for AssociatedTyName {
    fn generate<D: bolero::Driver>(driver: &mut D) -> Option<Self> {
        // Create an associated item id and look up its associated information
        let item_id: AssociatedItemId = driver.gen()?;
        let (trait_id, parameter_kinds) = FuzzCx::associated_id_map().get().get(&item_id)?.clone();
        Some(AssociatedTyName {
            trait_id,
            item_id,
            item_arity: parameter_kinds.len(),
        })
    }
}
