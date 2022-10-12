pub mod interned;

pub struct Id {
    data: &'static String,
}

pub fn id(s: &str) -> Id {
    unimplemented!()
}
