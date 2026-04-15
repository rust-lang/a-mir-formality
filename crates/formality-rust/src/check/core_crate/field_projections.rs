use crate::parse_term;

use super::LangItems;

pub fn lang_items() -> LangItems {
    parse_term! {
        trait Place {
            type Target: [];
        }

        unsafe trait Subplace {
            type Source: [];
            type Target: [];

            // FIXME: add metadata.
            fn offset(self: Self) -> usize;
        }

        unsafe trait PlaceRead<S>
        where
            Self: Place,
            S: Subplace,
            <S as Subplace>::Source => <Self as Place>::Target,
        {
            // FIXME: a-mir-formality doesn't support const trait items yet.
            // const SAFETY: bool;

            unsafe fn read(this: *const Self, proj: S) -> <S as Subplace>::Target;
        }

        unsafe trait PlaceWrite<S>
        where
            Self: Place,
            S: Subplace,
            <S as Subplace>::Source => <Self as Place>::Target,
        {
            // const SAFETY: bool;

            unsafe fn write(
                this: *const Self,
                sub: S,
                value: <S as Subplace>::Target
            ) -> ();
        }

        unsafe trait PlaceMove<S>
        where
            Self: PlaceRead<S>,
            S: Subplace,
            <S as Subplace>:: Source => <Self as Place>::Target,
        {
        }

        unsafe trait PlaceDrop<S>
        where
            Self: Place,
            S: Subplace,
            <S as Subplace>:: Source => <Self as Place>::Target,
        {
            unsafe fn drop(this: *const Self, sub: S) -> ();
        }

        unsafe trait DropHusk
        where
            Self: Place,
        {
            unsafe fn drop_husk(this: *const Self) -> ();
        }

        unsafe trait PlaceBorrow<S, X>
        where
            Self: Place,
            S: Subplace,
            <S as Subplace>::Source => <Self as Place>::Target,
        {

            // FIXME: a-mir-formality doesn't support const trait items yet.
            // const SAFETY: bool;

            // FIXME: a-mir-formality doesn't support const trait items yet.
            // const BORROW_KIND: BorrowKind;
            type BorrowDuration: [BorrowDuration];

            unsafe fn borrow(this: *const Self, sub: S) -> X;
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

        unsafe trait PlaceDeref<S>
        where
            Self: Place,
            S: Subplace,
            <S as Subplace>::Source => <Self as Place>::Target,
            <S as Subplace>::Target: Place,
        {
            unsafe fn deref(ptr: *mut Self, sub: S) -> *const <S as Subplace>::Target;
        }

        unsafe trait PlaceWrapper<S>
        where
            Self: Place,
            S: Subplace,
            <S as Subplace>::Source => <Self as Place>::Target,
        {
            type Wrapped: [Subplace]
            where
                <<Self as PlaceWrapper<S>>::Wrapped as Subplace>::Source => Self;

            fn wrap(sub: S) -> <Self as PlaceWrapper<S>>::Wrapped;
        }
    }
}
