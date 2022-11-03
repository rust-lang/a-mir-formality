use crate::grammar::{self as rust};
use formality_decl::grammar as decl;
use formality_types::grammar::{Binder, Fallible};

impl rust::Fn {
    pub(super) fn to_decl(&self) -> Fallible<decl::Fn> {
        let rust::Fn { id, binder } = self;

        let (
            names,
            rust::FnBoundData {
                input_tys,
                output_ty,
                where_clauses,
                body: _, // FIXME: function bodies
            },
        ) = binder.open();

        Ok(decl::Fn {
            id: id.clone(),
            binder: Binder::new(
                &names,
                decl::FnBoundData {
                    input_tys,
                    output_ty,
                    where_clauses: where_clauses
                        .iter()
                        .map(|wc| wc.to_decl())
                        .collect::<Fallible<_>>()?,
                },
            ),
        })
    }
}
