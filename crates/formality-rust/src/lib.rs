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
            "let",
            "in",
            "loop",
            "break",
            "continue",
            "return",

            "fn_id",
            "exists",
            "call",
            "print",
        ];
    }
}
// ANCHOR_END: declare_rust_language

// ANCHOR: use_rust_language
/// Declare the language that we will use in `#[term]` macros.
pub use rust::FormalityLang;
// ANCHOR_END: use_rust_language

pub mod check;
pub mod codegen;
pub mod codegen_judgment;
pub mod grammar;
pub mod prove;
mod test;
pub mod to_rust;
mod trait_binder;
pub mod types;

#[macro_export]
macro_rules! parse_term {
    ($($term:tt)*) => {
        $crate::rust::term(::core::stringify!($($term)*))
    };
}
