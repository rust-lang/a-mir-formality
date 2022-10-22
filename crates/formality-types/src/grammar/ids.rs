#[macro_export]
macro_rules! id {
    ($n:ident) => {
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $n {
            data: std::sync::Arc<String>,
        }

        const _: () = {
            use $crate::fold::{self, Fold};
            use $crate::grammar::Variable;
            use $crate::parse::{self, Parse};

            impl $n {
                fn new(s: String) -> $n {
                    $n {
                        data: std::sync::Arc::new(s),
                    }
                }
            }

            impl std::ops::Deref for $n {
                type Target = String;

                fn deref(&self) -> &String {
                    &self.data
                }
            }

            impl Fold for $n {
                fn substitute(&self, _substitution_fn: fold::SubstitutionFn<'_>) -> Self {
                    self.clone()
                }

                fn free_variables(&self) -> Vec<Variable> {
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
    };
}

id!(FnId);
id!(AdtId);
id!(TraitId);
id!(AssociatedItemId);
id!(CrateId);
id!(FieldId);
