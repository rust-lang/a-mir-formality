mod grammar;
mod type_system;

formality_core::declare_language! {
    mod eg {
        const NAME = "Eg";
        type Kind = crate::grammar::Kind;
        type Parameter = crate::grammar::Parameter;
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
use eg::FormalityLang;
use formality_core::Fallible;

fn main() -> Fallible<()> {
    Ok(())
}
