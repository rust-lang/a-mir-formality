/// The definition of a "language"
pub trait Language: 'static + Copy + Ord {
    /// Name of the language, e.g., `"Rust"`
    const NAME: &'static str;

    /// An enum defining the *kinds* of generic parameters (e.g., for Rust,
    /// types, lifetimes, and constants).
    type Kind;

    /// An enum defining the *value* of a generic parameter (e.g., a
    /// type, a lifetime, etc)
    type Parameter;
}
