use crate::cast::UpcastFrom;
use crate::term::CoreTerm;
use crate::variable::{CoreBoundVar, CoreExistentialVar, CoreUniversalVar, CoreVariable};
use std::fmt::Debug;
use std::hash::Hash;

/// The definition of a "language"
pub trait Language: 'static + Copy + Ord + Hash + Debug + Default {
    /// Name of the language, e.g., `"Rust"`
    const NAME: &'static str;

    /// An enum defining the *kinds* of generic parameters (e.g., for Rust,
    /// types, lifetimes, and constants).
    type Kind: Copy + CoreTerm<Self>;

    /// An enum defining the *value* of a generic parameter (e.g., a
    /// type, a lifetime, etc)
    type Parameter: HasKind<Self>
        + CoreTerm<Self>
        + UpcastFrom<CoreVariable<Self>>
        + UpcastFrom<CoreUniversalVar<Self>>
        + UpcastFrom<CoreExistentialVar<Self>>
        + UpcastFrom<CoreBoundVar<Self>>;

    /// The character (typically `<`) used to open binders.
    const BINDING_OPEN: char;

    /// The character (typically `>`) used to open binders.
    const BINDING_CLOSE: char;
}

/// For consistency with types like `CoreVariable<L>`, we write `CoreKind<L>` instead of `Kind<L>`.
pub type CoreKind<L: Language> = L::Kind;

/// For consistency with types like `CoreVariable<L>`, we write `CoreParameter<L>` instead of `Parameter<L>`.
pub type CoreParameter<L: Language> = L::Parameter;

pub trait HasKind<L: Language> {
    fn kind(&self) -> CoreKind<L>;
}
