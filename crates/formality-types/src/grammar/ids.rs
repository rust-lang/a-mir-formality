use crate::fold::{Fold, SubstitutionFn};
use formality_core::interned::Interned;

macro_rules! id {
    ($n:ident) => {
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $n {
            data: Interned<String>,
        }

        fn $n(s: &str) -> $n {
            let t = s.to_string();
            $n {
                data: Interned::from(t),
            }
        }

        impl std::ops::Deref for $n {
            type Target = String;

            fn deref(&self) -> &String {
                &self.data
            }
        }

        impl Fold for $n {
            fn substitute(&self, _substitution_fn: SubstitutionFn<'_>) -> Self {
                self.clone()
            }

            fn free_variables(&self) -> Vec<super::Variable> {
                vec![]
            }
        }
    };
}

id!(FnId);
id!(AdtId);
id!(TraitId);
id!(AssociatedItemId);
