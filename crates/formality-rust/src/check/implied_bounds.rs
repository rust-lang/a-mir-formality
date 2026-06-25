use crate::{
    grammar::{InputArg, Parameter, RigidName, RigidTy, Ty, Wcs, WhereClause},
    prove::ToWcs,
};

pub fn implied_outlives_for_fn(input_args: &Vec<InputArg>, output_ty: &Ty) -> Wcs {
    let mut implied_outlives: Vec<WhereClause> = input_args
        .iter()
        .flat_map(|arg| implied_outlives_from_ty(&arg.ty))
        .collect();
    implied_outlives.extend(implied_outlives_from_ty(output_ty));

    implied_outlives.to_wcs()
}

fn implied_outlives_from_ty(ty: &Ty) -> Vec<WhereClause> {
    let Ty::RigidTy(rigid_ty) = ty else {
        return vec![];
    };

    match rigid_ty {
        RigidTy {
            name: RigidName::Ref(_),
            parameters,
        } => {
            let Parameter::Lt(lt) = parameters[0].clone() else {
                return vec![];
            };
            let inner = &parameters[1];

            let mut wcs = vec![WhereClause::Outlives(inner.clone(), (*lt).clone())];

            if let Parameter::Ty(inner_ty) = inner {
                wcs.extend(implied_outlives_from_ty(inner_ty))
            }

            wcs
        }
        RigidTy {
            name: _,
            parameters,
        } => parameters
            .iter()
            .filter_map(|p| {
                if let Parameter::Ty(t) = p {
                    Some(implied_outlives_from_ty(t))
                } else {
                    None
                }
            })
            .flatten()
            .collect(),
    }
}
