use formality_core::interned::{Interned, Interner};

macro_rules! id {
    ($n:ident) => {
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $n {
            data: Interned<String>,
        }

        fn $n(s: &str) -> $n {
            lazy_static::lazy_static! {
                static ref I: Interner<String> = Interner::default();
            }
            let t = s.to_string();
            $n { data: I.intern(&t) }
        }

        impl std::ops::Deref for $n {
            type Target = String;

            fn deref(&self) -> &String {
                &self.data
            }
        }
    };
}

id!(VarId);
id!(FnId);
id!(AdtId);
id!(TraitId);
id!(AssociatedItemId);
