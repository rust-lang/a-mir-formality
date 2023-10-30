use crate::cast::UpcastFrom;
use crate::derive_links::{Term, Variable};
use crate::variable::{BoundVar, ExistentialVar, UniversalVar};
use std::fmt::Debug;
use std::hash::Hash;

/// The definition of a "language"
pub trait Language: 'static + Copy + Ord + Hash + Debug + Default {
    /// Name of the language, e.g., `"Rust"`
    const NAME: &'static str;

    /// An enum defining the *kinds* of generic parameters (e.g., for Rust,
    /// types, lifetimes, and constants).
    type Kind: Copy + Term<Self>;

    /// An enum defining the *value* of a generic parameter (e.g., a
    /// type, a lifetime, etc)
    type Parameter: HasKind<Self>
        + Term<Self>
        + UpcastFrom<Variable<Self>>
        + UpcastFrom<UniversalVar<Self>>
        + UpcastFrom<ExistentialVar<Self>>
        + UpcastFrom<BoundVar<Self>>;

    /// The character (typically `<`) used to open binders.
    const BINDING_OPEN: char;

    /// The character (typically `>`) used to open binders.
    const BINDING_CLOSE: char;
}

/// For consistency with types like `Variable<L>`, we write `Kind<L>` instead of `Kind<L>`.
pub type Kind<L: Language> = L::Kind;

/// For consistency with types like `Variable<L>`, we write `Parameter<L>` instead of `Parameter<L>`.
pub type Parameter<L: Language> = L::Parameter;

pub trait HasKind<L: Language> {
    fn kind(&self) -> Kind<L>;
}
