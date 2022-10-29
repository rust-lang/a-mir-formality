use crate::grammar::{self as rust};
use formality_decl::grammar::{self as decl, VariantId};
use formality_types::grammar::{Binder, Fallible};

impl rust::Struct {
    pub(crate) fn to_decl(&self) -> Fallible<decl::Adt> {
        let rust::Struct { id, binder } = self;

        let (
            vars,
            rust::StructBoundData {
                where_clauses,
                fields,
            },
        ) = binder.open();

        Ok(decl::Adt {
            kind: decl::AdtKind::Struct,
            id: id.clone(),
            binder: Binder::new(
                &vars,
                decl::AdtBoundData {
                    where_clauses: where_clauses
                        .iter()
                        .map(|wc| wc.to_decl())
                        .collect::<Fallible<_>>()?,
                    variants: vec![decl::AdtVariant {
                        name: VariantId::new("struct"),
                        fields: fields
                            .iter()
                            .map(|f| f.to_decl())
                            .collect::<Fallible<_>>()?,
                    }],
                },
            ),
        })
    }
}
