use crate::fold::{Fold, SubstitutionFn};
use crate::parse::{self, Parse};
use std::sync::Arc;

macro_rules! id {
    ($n:ident) => {
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $n {
            data: Arc<String>,
        }

        impl $n {
            fn new(s: String) -> $n {
                $n { data: Arc::new(s) }
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

        impl Parse for $n {
            fn parse<'t>(_scope: &parse::Scope, text: &'t str) -> Option<(Self, &'t str)> {
                let (string, text) = parse::identifier(text)?;
                let n = $n::new(string);
                Some((n, text))
            }
        }
    };
}

id!(FnId);
id!(AdtId);
id!(TraitId);
id!(AssociatedItemId);
id!(CrateId);
id!(FieldId);
