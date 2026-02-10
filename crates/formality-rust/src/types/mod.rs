#![allow(dead_code)]

pub mod grammar;

// ANCHOR: declare_rust_language
formality_core::declare_language! {
    pub mod rust {
        const NAME = "Rust";
        type Kind = crate::types::grammar::ParameterKind;
        type Parameter = crate::types::grammar::Parameter;
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
use crate::types::rust::FormalityLang;
// ANCHOR_END: use_rust_language
