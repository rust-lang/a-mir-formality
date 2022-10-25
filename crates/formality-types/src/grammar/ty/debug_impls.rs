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

impl std::fmt::Debug for super::Lt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.data())
    }
}

impl std::fmt::Debug for super::Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PlaceholderVar(arg0) => write!(f, "{:?}", arg0),
            Self::InferenceVar(arg0) => write!(f, "{:?}", arg0),
            Self::BoundVar(arg0) => write!(f, "{:?}", arg0),
        }
    }
}

impl std::fmt::Debug for super::PlaceholderVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let super::PlaceholderVar {
            universe,
            var_index,
        } = self;
        write!(f, "!{:?}_{:?}", universe, var_index)
    }
}

impl std::fmt::Debug for super::InferenceVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let super::InferenceVar { index } = self;
        write!(f, "?{:?}", index)
    }
}

impl std::fmt::Debug for super::VarIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.index)
    }
}

impl std::fmt::Debug for super::BoundVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            super::BoundVar {
                debruijn: None,
                var_index,
            } => write!(f, "_{:?}", var_index),
            super::BoundVar {
                debruijn: Some(db),
                var_index,
            } => write!(f, "{:?}_{:?}", db, var_index),
        }
    }
}

impl std::fmt::Debug for super::DebruijnIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "^{}", self.index)
    }
}
