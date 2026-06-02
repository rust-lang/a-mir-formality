use crate::parse_term;

use super::LangItems;

pub fn lang_items() -> LangItems {
    parse_term! {
        // Marks a type as dereferencable.
        //
        // When computing the type of a place expression `*x`, the type of `x` must implement this
        // trait.
        trait Derefable {
            type Target: [];
        }

        /*
        unsafe trait Subplace {
            type Source: [];
            type Target: [];

            // FIXME: add metadata.
            fn offset(self: Self) -> usize;
        }

        // The duration of the loan tracked by the borrow checker.
        //
        // The borrow checker ensures that `this` has `ACCESS` rights to `S`.
        trait BorrowDuration {}

        // Types implementing `BorrowDuration`:

        // For only the duration of the `PlaceBorrow::borrow` call.
        struct Instant {}
        // For the duration of `'a`.
        struct Lifetime<'a> {}
        // Until the pointer is dropped.
        struct Indefinite {}

        impl BorrowDuration for Instant {}
        impl<'a> BorrowDuration for Lifetime<'a> {}
        impl BorrowDuration for Indefinite {}
        */
    }
}
