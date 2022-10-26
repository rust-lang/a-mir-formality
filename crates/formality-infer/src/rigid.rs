use formality_types::{
    cast::Upcast,
    db::Db,
    grammar::{ParameterKind, RefKind, RigidName, RigidTy, Ty, Universe, Variance},
};

use crate::Env;

impl Env {
    pub(crate) fn fresh_rigid_ty(&mut self, db: &Db, name: &RigidName, universe: Universe) -> Ty {
        let parameters: Vec<_> = self
            .rigid_generics(db, name)
            .into_iter()
            .map(|(kind, _)| {
                self.next_inference_variable(kind, universe)
                    .into_parameter(kind)
            })
            .collect();

        RigidTy {
            name: name.clone(),
            parameters,
        }
        .upcast()
    }

    fn rigid_generics(&self, _db: &Db, name: &RigidName) -> Vec<(ParameterKind, Variance)> {
        use ParameterKind::*;
        use Variance::*;
        match name {
            RigidName::AdtId(_) => unimplemented!("adt variance"),
            RigidName::ScalarId(_) => vec![],
            RigidName::Ref(RefKind::Shared) => vec![(Lt, Covariant), (Ty, Covariant)],
            RigidName::Ref(RefKind::Mut) => vec![(Lt, Covariant), (Ty, Invariant)],
            RigidName::Tuple(n) => (0..*n).map(|_| (Ty, Covariant)).collect(),
            RigidName::FnPtr(n_args) => (0..*n_args)
                .map(|_| (Ty, Contravariant))
                .chain(Some((Ty, Covariant)))
                .collect(),
            RigidName::FnDef(_) => unimplemented!("fndef variance"),
        }
    }
}
