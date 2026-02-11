// ANCHOR: declare_rust_language
formality_core::declare_language! {
    pub mod rust {
        const NAME = "Rust";
        type Kind = crate::grammar::ParameterKind;
        type Parameter = crate::grammar::Parameter;
        const BINDING_OPEN = '<';
        const BINDING_CLOSE = '>';
        const KEYWORDS = [
            "mut",
            "struct",
            "enum",
            "union",
            "const",
            "true",
            "false",
            "static",
        ];
    }
}
// ANCHOR_END: declare_rust_language

/// Declare the language that we will use in `#[term]` macros.
pub use rust::FormalityLang;

pub mod check;
pub mod grammar;
pub mod prove;
mod test;
mod trait_binder;
pub mod types;
