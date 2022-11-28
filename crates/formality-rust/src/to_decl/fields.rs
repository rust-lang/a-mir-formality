use crate::grammar as rust;
use formality_decl::grammar as decl;
use formality_types::grammar::Fallible;

impl rust::Field {
    pub(super) fn to_decl(&self) -> Fallible<decl::Field> {
        let rust::Field { name, ty } = self;
        Ok(decl::Field {
            name: name.clone(),
            ty: ty.clone(),
        })
    }
}
