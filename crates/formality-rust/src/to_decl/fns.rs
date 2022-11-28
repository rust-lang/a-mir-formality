use crate::grammar::{self as rust};
use formality_decl::grammar as decl;
use formality_types::{
    cast::Upcast,
    grammar::{Binder, Fallible, Predicate, Ty},
    seq,
};

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

        let implied_bounds = self.fn_implied_bounds(&input_tys, &output_ty);

        let explicit_bounds: Vec<Predicate> = where_clauses
            .iter()
            .map(|wc| wc.to_decl())
            .collect::<Fallible<_>>()?;

        Ok(decl::Fn {
            id: id.clone(),
            binder: Binder::new(
                &names,
                decl::FnBoundData {
                    input_tys,
                    output_ty,
                    where_clauses: seq![..implied_bounds, ..explicit_bounds],
                },
            ),
        })
    }

    fn fn_implied_bounds(&self, input_tys: &[Ty], output_ty: &Ty) -> Vec<Predicate> {
        input_tys
            .iter()
            .chain(Some(output_ty))
            .map(|ty| ty.well_formed().upcast())
            .collect()
    }
}
