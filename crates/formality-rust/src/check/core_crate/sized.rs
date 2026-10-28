use crate::parse_term;

use super::LangItems;

pub fn lang_items() -> LangItems {
    parse_term! {
        trait Sized {}
        impl Sized for () {}
        impl Sized for u8 {}
        impl Sized for u16 {}
        impl Sized for u32 {}
        impl Sized for u64 {}
        impl Sized for u128 {}
        impl Sized for i8 {}
        impl Sized for i16 {}
        impl Sized for i32 {}
        impl Sized for i64 {}
        impl Sized for i128 {}
        impl Sized for bool {}
        impl Sized for usize {}
        impl Sized for isize {}
        impl<'a, T> Sized for &'a T {}
        impl<'a, T> Sized for &'a mut T {}
    }
}
