use crate::grammar::Const;

impl std::fmt::Debug for super::Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            super::TyData::RigidTy(r) => write!(f, "{r:?}"),
            super::TyData::AliasTy(r) => write!(f, "{r:?}"),
            super::TyData::PredicateTy(r) => write!(f, "{r:?}"),
            super::TyData::Variable(r) => write!(f, "{r:?}"),
        }
    }
}

impl std::fmt::Debug for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            crate::grammar::ConstData::Value(valtree, ty) => write!(f, "{valtree:?}_{ty:?}"),
            crate::grammar::ConstData::Variable(r) => write!(f, "{r:?}"),
        }
    }
}

impl std::fmt::Debug for super::Lt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            super::LtData::Static => write!(f, "static"),
            super::LtData::Variable(v) => write!(f, "{:?}", v),
        }
    }
}
