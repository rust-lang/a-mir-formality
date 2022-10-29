use crate::grammar as rust;
use formality_decl::grammar as decl;
use formality_types::grammar::Fallible;

impl rust::Variant {
    pub(crate) fn to_decl(&self) -> Fallible<decl::AdtVariant> {
        let rust::Variant { name, fields } = self;

        Ok(decl::AdtVariant {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|field| field.to_decl())
                .collect::<Fallible<_>>()?,
        })
    }
}
