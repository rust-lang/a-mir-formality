mod ambiguity;
mod commit_points;
mod grammar;
mod left_associative;
mod none_associative;
mod path;
mod right_associative;

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

/// Used to parse `text` when we expect some remainder
fn expect_remainder<T>(text: &str) -> (T, &str)
where
    T: CoreParse<FormalityLang>,
{
    match T::parse(&Default::default(), text) {
        Ok(parse) => {
            let (value, remainder) = parse.finish();
            assert!(
                !remainder.is_empty(),
                "expected to have remainder text, but parsed entire term `{text:?}`"
            );
            (value, remainder)
        }
        Err(errs) => panic!("encountered unexpected parse error: {errs:#?}"),
    }
}

// Default language for our crate
use formality_core::{parse::CoreParse, Fallible};
use ptt::FormalityLang;

fn main() -> Fallible<()> {
    Ok(())
}
