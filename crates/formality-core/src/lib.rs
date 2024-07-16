//! `formality-core` contains core definitions that can be used for
//! languages that are not Rust. It is intended to play a role similar
//! to
//!

#![allow(type_alias_bounds)]

extern crate self as formality_core;

// Re-export things from dependencies to avoid everybody repeating same set
// in their Cargo.toml.
pub use anyhow::anyhow;
pub use anyhow::bail;
pub use bolero;
pub use contracts::requires;
pub use tracing::debug;
pub use tracing::instrument;
pub use tracing::trace;

// Re-export things from formality-macros.
pub use formality_macros::{fixed_point, respan, term, test, Fuzz, Visit};

pub type Fallible<T> = anyhow::Result<T>;

// Modules are *pub* if the items they export aren't meant to be directly
// used, or at least not most of the time. The idea is that you should use
// the names from `declare_language`.
pub mod binder;
mod cast;
mod collections;
pub mod fixed_point;
pub mod fold;
pub mod fuzz;
pub mod judgment;
pub mod language;
pub mod parse;
pub mod substitution;
pub mod term;
pub mod test_util;
pub mod util;
pub mod variable;
pub mod visit;

pub use cast::{Downcast, DowncastFrom, DowncastTo, Downcasted, To, Upcast, UpcastFrom, Upcasted};
pub use collections::Cons;
pub use collections::Deduplicate;
pub use collections::Map;
pub use collections::Set;
pub use collections::SetExt;
pub use judgment::ProvenSet;

/// Run an action with a tracing log subscriber. The logging level is loaded
/// from `RUST_LOG`. The `formality_macro::test` expansion uses this to enable logs.
pub fn with_tracing_logs<T>(action: impl FnOnce() -> T) -> T {
    use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};
    use tracing_tree::HierarchicalLayer;
    let filter = EnvFilter::from_env("RUST_LOG");
    let subscriber = Registry::default()
        .with(filter)
        .with(HierarchicalLayer::new(2).with_writer(std::io::stdout));
    tracing::subscriber::with_default(subscriber, action)
}

#[macro_export]
macro_rules! trait_alias {
    (
        $(#[$m:meta])*
        $v:vis trait $t:ident = $($u:tt)*
    ) => {
        $($m)*
        $v trait $t: $($u)* { }

        impl<T> $t for T
        where
            T: $($u)*
        { }
    }
}

/// Declares a new formality language.
/// This will generate a module with a name you choose that contains various items;
/// among them will be a struct named `FormalityLang` that implements the
/// [`Language`](`crate::language::Language`) trait.
/// When you use the auto-derives or the [`term`](`crate::term`) macro, they will generate
/// code that references `crate::FormalityLang`, so you need to bring that in scope at the root
/// of your crate (e.g., if you called the module `mylang`, you might
/// add `use crate::mylang::FormalityLang` at the root of your crate,
/// so that the auto-derives can find it.)
///
/// See the mdbook for more coverage of how it works.
#[macro_export]
macro_rules! declare_language {
    (
        $(#[$the_lang_m:meta])*
        $the_lang_v:vis mod $the_lang:ident {
            const NAME = $name:expr;
            type Kind = $kind:ty;
            type Parameter = $param:ty;
            const BINDING_OPEN = $binding_open:expr;
            const BINDING_CLOSE = $binding_close:expr;
            const KEYWORDS = [$($kw:expr),* $(,)?];
        }
    ) => {
        $(#[$the_lang_m:meta])*
        $the_lang_v mod $the_lang {
            use $crate::language::Language;

            #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
            pub struct FormalityLang;

            // This module may seem weird -- it permits us to import `super::*`
            // so that all the types in `$kind` and `$param` are valid without
            // importing `super::*` into the entire module. This not only makes
            // things a bit nicer, since those imports are not needed and could
            // cause weird behavior, it avoids a cycle when users
            // do `pub use $lang::grammar::*`.
            mod __hygiene {
                use super::super::*;
                impl $crate::language::Language for super::FormalityLang {
                    const NAME: &'static str = $name;

                    type Kind = $kind;

                    type Parameter = $param;

                    const BINDING_OPEN: char = $binding_open;

                    const BINDING_CLOSE: char = $binding_close;

                    const KEYWORDS: &'static [&'static str] = &[$($kw),*];
                }
            }

            $crate::trait_alias! {
                pub trait Fold = $crate::fold::CoreFold<FormalityLang>
            }

            $crate::trait_alias! {
                pub trait Visit = $crate::visit::CoreVisit<FormalityLang>
            }

            $crate::trait_alias! {
                pub trait Parse = $crate::parse::CoreParse<FormalityLang>
            }

            $crate::trait_alias! {
                pub trait Term = $crate::term::CoreTerm<FormalityLang>
            }

            /// Grammar items to be included in this language.
            pub mod grammar {
                use super::FormalityLang;
                pub type Variable = $crate::variable::CoreVariable<FormalityLang>;
                pub type ExistentialVar = $crate::variable::CoreExistentialVar<FormalityLang>;
                pub type UniversalVar = $crate::variable::CoreUniversalVar<FormalityLang>;
                pub type BoundVar = $crate::variable::CoreBoundVar<FormalityLang>;
                pub type DebruijnIndex = $crate::variable::DebruijnIndex;
                pub type VarIndex = $crate::variable::VarIndex;
                pub type Binder<T> = $crate::binder::CoreBinder<FormalityLang, T>;
                pub type Substitution = $crate::substitution::CoreSubstitution<FormalityLang>;
                pub type VarSubstitution = $crate::substitution::CoreVarSubstitution<FormalityLang>;
            }

            /// Parses `text` as a term with no bindings in scope.
            #[track_caller]
            pub fn term<T>(text: &str) -> T
            where
                T: Parse,
            {
                try_term(text).unwrap()
            }

            /// Parses `text` as a term with no bindings in scope.
            #[track_caller]
            pub fn try_term<T>(text: &str) -> $crate::Fallible<T>
            where
                T: Parse,
            {
                term_with(None::<(String, grammar::Variable)>, text)
            }

            /// Parses `text` as a term with the given bindings in scope.
            ///
            /// References to the given string will be replaced with the given parameter
            /// when parsing types, lifetimes, etc.
            #[track_caller]
            pub fn term_with<T, B>(bindings: impl IntoIterator<Item = B>, text: &str) -> $crate::Fallible<T>
            where
                T: Parse,
                B: $crate::Upcast<(String, grammar::Variable)>,
            {
                $crate::parse::core_term_with::<FormalityLang, T, B>(bindings, text)
            }
        }
    }
}

/// Declares a newtyped struct that is an arbitrary (string) identifier,
/// like the name of a module or something.
#[macro_export]
macro_rules! id {
    ($n:ident) => {
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $n {
            data: std::sync::Arc<String>,
        }

        const _: () = {
            use $crate::fold::{self, CoreFold};
            use $crate::parse::{self, CoreParse};
            use $crate::variable::CoreVariable;
            use $crate::visit::CoreVisit;

            $crate::cast_impl!($n);

            impl $n {
                pub fn new(s: &str) -> $n {
                    $n {
                        data: std::sync::Arc::new(s.to_string()),
                    }
                }
            }

            impl<L: $crate::language::Language> $crate::fuzz::Fuzzable<L> for $n {
                fn estimate_cardinality(cx: &mut $crate::fuzz::FuzzCx<'_, L>) -> f64 {
                    cx.enter_estimate_cardinality::<Self>(|guard| {
                        guard.num_available_values() as f64
                    })
                }

                fn fuzz(cx: &mut $crate::fuzz::FuzzCx<'_, L>) -> Option<Self> {
                    cx.enter_fuzz::<Self>(|guard| guard.pick_value())
                }
            }

            impl std::ops::Deref for $n {
                type Target = String;

                fn deref(&self) -> &String {
                    &self.data
                }
            }

            impl CoreVisit<crate::FormalityLang> for $n {
                fn free_variables(&self) -> Vec<CoreVariable<crate::FormalityLang>> {
                    vec![]
                }

                fn size(&self) -> usize {
                    1
                }

                fn assert_valid(&self) {}
            }

            impl CoreFold<crate::FormalityLang> for $n {
                fn substitute(
                    &self,
                    _substitution_fn: fold::SubstitutionFn<'_, crate::FormalityLang>,
                ) -> Self {
                    self.clone()
                }
            }

            impl CoreParse<crate::FormalityLang> for $n {
                fn parse<'t>(
                    scope: &parse::Scope<crate::FormalityLang>,
                    text: &'t str,
                ) -> parse::ParseResult<'t, Self> {
                    $crate::parse::Parser::single_variant(scope, text, stringify!($n), |p| {
                        let string = p.identifier()?;
                        Ok($n::new(&string))
                    })
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
