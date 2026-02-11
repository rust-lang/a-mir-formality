use crate::grammar::WhereClause;
use crate::grammar::{AdtId, Binder, FieldId, Ty, VariantId};
use crate::prove::prove::{AdtDeclField, AdtDeclFieldName, AdtDeclVariant};
use formality_core::term;

#[term(struct $id $binder)]
pub struct Struct {
    pub id: AdtId,
    pub binder: Binder<StructBoundData>,
}

impl Struct {
    pub fn to_adt(&self) -> Adt {
        let (
            vars,
            StructBoundData {
                where_clauses,
                fields,
            },
        ) = self.binder.open();
        Adt {
            id: self.id.clone(),
            binder: Binder::new(
                vars,
                AdtBoundData {
                    where_clauses,
                    variants: vec![Variant {
                        name: VariantId::for_struct(),
                        fields,
                    }],
                },
            ),
        }
    }
}

#[term($:where $,where_clauses { $,fields })]
pub struct StructBoundData {
    pub where_clauses: Vec<WhereClause>,
    pub fields: Vec<Field>,
}

#[term($name : $ty)]
pub struct Field {
    pub name: FieldName,
    pub ty: Ty,
}

impl Field {
    pub fn to_adt_decl_field(&self) -> AdtDeclField {
        return AdtDeclField {
            name: self.name.to_adt_decl_field_name(),
            ty: self.ty.clone(),
        };
    }
}

#[term]
pub enum FieldName {
    #[cast]
    Id(FieldId),
    #[cast]
    Index(usize),
}

impl FieldName {
    pub fn to_adt_decl_field_name(&self) -> AdtDeclFieldName {
        match self {
            FieldName::Id(field_id) => {
                return AdtDeclFieldName::Id(field_id.clone());
            }
            FieldName::Index(idx) => {
                return AdtDeclFieldName::Index(*idx);
            }
        }
    }
}

#[term(enum $id $binder)]
pub struct Enum {
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

impl Enum {
    pub fn to_adt(&self) -> Adt {
        Adt {
            id: self.id.clone(),
            binder: self.binder.clone(),
        }
    }
}

/// Not directly part of the grammar, but structs/enums
/// can be converted to this.
#[term(adt $id $binder)]
pub struct Adt {
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

#[term($:where $,where_clauses { $,variants })]
pub struct AdtBoundData {
    pub where_clauses: Vec<WhereClause>,
    pub variants: Vec<Variant>,
}

#[term($name { $,fields })]
pub struct Variant {
    pub name: VariantId,
    pub fields: Vec<Field>,
}

impl Variant {
    pub fn to_adt_decl_variant(&self) -> AdtDeclVariant {
        AdtDeclVariant {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|field| field.to_adt_decl_field())
                .collect(),
        }
    }
}
