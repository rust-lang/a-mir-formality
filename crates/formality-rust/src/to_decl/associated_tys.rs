use crate::grammar::{self as rust};
use formality_decl::grammar as decl;
use formality_types::{
    cast::To,
    grammar::{fresh_bound_var, Binder, Fallible, ParameterKind},
};

impl rust::AssociatedTy {
    pub(super) fn to_decl(&self) -> Fallible<decl::AssociatedTy> {
        let rust::AssociatedTy { id, binder } = self;

        let (
            names,
            rust::AssociatedTyBoundData {
                ensures,
                where_clauses,
            },
        ) = binder.open();

        let bound_var = fresh_bound_var(ParameterKind::Ty);
        let ensures = Binder::new(
            &[bound_var],
            ensures
                .iter()
                .map(|e| e.to_decl(&bound_var.to()))
                .collect::<Fallible<_>>()?,
        );

        Ok(decl::AssociatedTy {
            id: id.clone(),
            binder: Binder::new(
                &names,
                decl::AssociatedTyBoundData {
                    ensures,
                    where_clauses: where_clauses
                        .iter()
                        .map(|wc| wc.to_decl())
                        .collect::<Fallible<_>>()?,
                },
            ),
        })
    }
}

impl rust::AssociatedTyValue {
    pub(super) fn to_decl(&self) -> Fallible<decl::AssociatedTyValue> {
        let rust::AssociatedTyValue { id, binder } = self;

        let (names, rust::AssociatedTyValueBoundData { where_clauses, ty }) = binder.open();

        Ok(decl::AssociatedTyValue {
            id: id.clone(),
            binder: Binder::new(
                &names,
                decl::AssociatedTyValueBoundData {
                    where_clauses: where_clauses
                        .iter()
                        .map(|wc| wc.to_decl())
                        .collect::<Fallible<_>>()?,
                    ty,
                },
            ),
        })
    }
}
