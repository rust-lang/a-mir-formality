use formality_core::Fallible;

formality_core::declare_language! {
    mod jer {
        const NAME = "JER";
        type Kind = crate::grammar::DummyKind;
        type Parameter = crate::grammar::DummyParameter;
        const BINDING_OPEN = '<';
        const BINDING_CLOSE = '>';
        const KEYWORDS = [
            "struct",
            "fn",
            "let",
            "in",
            "integer",
        ];
    }
}

use jer::FormalityLang;

mod cyclic_judgment;
mod fallible;
mod grammar;

fn main() -> Fallible<()> {
    Ok(())
}
