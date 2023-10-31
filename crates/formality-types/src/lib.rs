#![allow(dead_code)]

pub mod fold;
pub mod grammar;
pub mod visit;

formality_core::declare_language! {
    pub mod rust {
        const NAME = "Rust";
        type Kind = crate::grammar::ParameterKind;
        type Parameter = crate::grammar::Parameter;
        const BINDING_OPEN = '<';
        const BINDING_CLOSE = '>';
    }
}

use crate::rust::FormalityLang;
