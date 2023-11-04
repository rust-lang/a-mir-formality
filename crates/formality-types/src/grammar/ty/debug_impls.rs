use crate::grammar::Const;

impl std::fmt::Debug for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            crate::grammar::ConstData::Value(valtree, ty) => write!(f, "{valtree:?}_{ty:?}"),
            crate::grammar::ConstData::Variable(r) => write!(f, "{r:?}"),
        }
    }
}
