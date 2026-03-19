use crate::grammar::WhereClause;
use crate::grammar::{AdtId, Binder, FieldId, Ty, VariantId};
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

#[term]
pub enum FieldName {
    #[cast]
    Id(FieldId),
    #[cast]
    Index(usize),
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

impl AdtBoundData {
    pub fn struct_variant(&self) -> &Variant {
        assert_eq!(self.variants.len(), 1);
        &self.variants[0]
    }
}

#[term($name { $,fields })]
pub struct Variant {
    pub name: VariantId,
    pub fields: Vec<Field>,
}
