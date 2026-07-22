use crate::grammar::{expr::PlaceExpr, AdtId, FieldName, Ty, ValueId, VariantId};
use formality_core::{cast_impl, term, DowncastTo};
use std::sync::Arc;

#[term($data: $ty)]
pub struct TypedPlaceExpr {
    pub ty: Ty,
    pub data: Arc<TypedPlaceExpressionData>,
}

cast_impl!((ValueId) <: (TypedPlaceExpressionData) <: (Arc<TypedPlaceExpressionData>));

#[term]
pub enum TypedPlaceExpressionData {
    #[cast]
    Local(ValueId),

    #[grammar(*($v0))]
    Deref(TypedPlaceExpr),

    // Project to a field.
    #[grammar($v0.$v1 [$v2 , $v3])]
    Field(TypedPlaceExpr, FieldName, AdtId, VariantId),
    // Index
    // Downcast
    #[grammar($v0.$v1)]
    TupleField(TypedPlaceExpr, usize),
}

impl DowncastTo<TypedPlaceExpressionData> for TypedPlaceExpr {
    fn downcast_to(&self) -> Option<TypedPlaceExpressionData> {
        Some(self.data().clone())
    }
}

impl TypedPlaceExpr {
    pub fn data(&self) -> &TypedPlaceExpressionData {
        &self.data
    }

    /// Convert to an untyped PlaceExpression
    pub fn to_place_expression(&self) -> PlaceExpr {
        match self.data() {
            TypedPlaceExpressionData::Local(id) => PlaceExpr::var(id),
            TypedPlaceExpressionData::Deref(inner) => PlaceExpr::deref(inner.to_place_expression()),
            TypedPlaceExpressionData::Field(root, field, _, _) => {
                PlaceExpr::field(root.to_place_expression(), field)
            }
            TypedPlaceExpressionData::TupleField(root, field_index) => {
                PlaceExpr::field(root.to_place_expression(), field_index)
            }
        }
    }

    /// True if `self` is a prefix of `other`
    pub fn is_prefix_of(&self, other: &TypedPlaceExpr) -> bool {
        other.all_prefixes().contains(&self)
    }

    /// Returns all prefixes of `self`
    pub fn all_prefixes(&self) -> Vec<&TypedPlaceExpr> {
        // For now, we just return self and its prefixes without type info for inner prefixes
        // This is a simplification - we could track types through the chain if needed
        let mut v = vec![self];
        let mut current = self;
        while let Some(prefix) = current.prefix() {
            v.push(prefix);
            current = prefix;
        }
        v
    }

    /// Returns the next prefix of `self` (if any)
    pub fn prefix(&self) -> Option<&TypedPlaceExpr> {
        match self.data() {
            TypedPlaceExpressionData::Local(_) => None,
            TypedPlaceExpressionData::Deref(prefix)
            | TypedPlaceExpressionData::Field(prefix, _, _, _)
            | TypedPlaceExpressionData::TupleField(prefix, _) => Some(prefix),
        }
    }
}
