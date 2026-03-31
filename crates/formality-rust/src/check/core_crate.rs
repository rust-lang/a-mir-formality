use crate::grammar::Crate;

pub fn krate() -> Crate {
    Crate {
        id: "core".to_string().into(),
        items: vec![],
    }
}
