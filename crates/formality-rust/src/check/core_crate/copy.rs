use crate::parse_term;

use super::LangItems;

pub fn lang_items() -> LangItems {
    parse_term! {
        trait Copy {}
        impl Copy for u8 {}
        impl Copy for u16 {}
        impl Copy for u32 {}
        impl Copy for u64 {}
        impl Copy for i8 {}
        impl Copy for i16 {}
        impl Copy for i32 {}
        impl Copy for i64 {}
        impl Copy for bool {}
        impl Copy for usize {}
        impl Copy for isize {}
        impl<'a, T> Copy for &'a T {}
    }
}
