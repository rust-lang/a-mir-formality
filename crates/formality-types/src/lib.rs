#![allow(dead_code)]

pub mod fuzz;
pub mod grammar;

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

// ANCHOR: use_rust_language
use crate::rust::FormalityLang;
// ANCHOR_END: use_rust_language
