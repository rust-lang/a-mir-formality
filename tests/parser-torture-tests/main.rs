mod ambiguity;
mod grammar;
mod precedence;

formality_core::declare_language! {
    mod ptt {
        const NAME = "PTT";
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

// Default language for our crate
use formality_core::Fallible;
use ptt::FormalityLang;

fn main() -> Fallible<()> {
    Ok(())
}
