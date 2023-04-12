#[macro_export]
macro_rules! id {
    ($n:ident) => {
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $n {
            data: std::sync::Arc<String>,
        }

        const _: () = {
            use $crate::fold::{self, Fold};
            use $crate::grammar::Variable;
            use $crate::parse::{self, Parse};
            use $crate::visit::Visit;

            $crate::cast_impl!($n);

            impl $n {
                pub fn new(s: &str) -> $n {
                    $n::from(s.to_string())
                }
            }

            impl From<&str> for $n {
                fn from(v: &str) -> $n {
                    $n::new(v)
                }
            }

            impl From<String> for $n {
                fn from(v: String) -> $n {
                    $n {
                        data: std::sync::Arc::new(v),
                    }
                }
            }

            impl std::ops::Deref for $n {
                type Target = String;

                fn deref(&self) -> &String {
                    &self.data
                }
            }

            impl Visit for $n {
                fn free_variables(&self) -> Vec<Variable> {
                    vec![]
                }

                fn size(&self) -> usize {
                    1
                }

                fn assert_valid(&self) {}
            }

            impl Fold for $n {
                fn substitute(&self, _substitution_fn: fold::SubstitutionFn<'_>) -> Self {
                    self.clone()
                }
            }

            impl Parse for $n {
                fn parse<'t>(_scope: &parse::Scope, text: &'t str) -> parse::ParseResult<'t, Self> {
                    let (string, text) = parse::identifier(text)?;
                    let n = $n::new(&string);
                    Ok((n, text))
                }
            }

            impl std::fmt::Debug for $n {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", &self.data)
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
id!(VariantId);

impl VariantId {
    /// Returns the special variant-id used for the single variant of a struct.
    pub fn for_struct() -> Self {
        VariantId::new("struct")
    }
}
