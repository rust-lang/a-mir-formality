use crate::grammar::{self as rust};
use formality_decl::grammar::{self as decl};
use formality_types::grammar::{Binder, Fallible};

impl rust::Enum {
    pub(crate) fn to_decl(&self) -> Fallible<decl::Adt> {
        let rust::Enum { id, binder } = self;

        let (
            vars,
            rust::EnumBoundData {
                where_clauses,
                variants,
            },
        ) = binder.open();

        Ok(decl::Adt {
            kind: decl::AdtKind::Enum,
            id: id.clone(),
            binder: Binder::new(
                &vars,
                decl::AdtBoundData {
                    where_clauses: where_clauses
                        .iter()
                        .map(|wc| wc.to_decl())
                        .collect::<Fallible<_>>()?,
                    variants: variants
                        .iter()
                        .map(|variant| variant.to_decl())
                        .collect::<Fallible<_>>()?,
                },
            ),
        })
    }
}
