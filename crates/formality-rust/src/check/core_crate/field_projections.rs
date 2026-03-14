use crate::parse_term;

use super::LangItems;

pub fn lang_items() -> LangItems {
    parse_term! {
        trait HasPlace {
            type Target: [];
        }

        unsafe trait Projection {
            type Source: [];
            type Target: [];

            fn offset(self: Self) -> usize;
        }

        unsafe trait PlaceRead<P>
        where
            Self: HasPlace,
            P: Projection,
            <P as Projection>::Source => <Self as HasPlace>::Target,
        {
            // FIXME: a-mir-formality doesn't support const trait items yet.
            // const SAFETY: bool;

            unsafe fn read(this: *const Self, proj: P) -> <P as Projection>::Target;
        }

        unsafe trait PlaceWrite<P>
        where
            Self: HasPlace,
            P: Projection,
            <P as Projection>::Source => <Self as HasPlace>::Target,
        {
            // const SAFETY: bool;

            unsafe fn write(
                this: *const Self,
                proj: P,
                value: <P as Projection>::Target
            ) -> ();
        }

        unsafe trait PlaceMove<P>
        where
            // TODO: do we need this?
            // Self: HasPlace,
            Self: PlaceRead<P>,
            P: Projection,
            <P as Projection>:: Source => <Self as HasPlace>::Target,
        {
        }
        unsafe trait DropHusk
        where
            Self: HasPlace,
        {
            unsafe fn drop_husk(this: *const Self) -> ();
        }

        unsafe trait PlaceDrop<P>
        where
            Self: HasPlace,
            P: Projection,
            <P as Projection>:: Source => <Self as HasPlace>::Target,
        {
            unsafe fn drop(this: *const Self, proj: P) -> ();
        }

        unsafe trait PlaceBorrow<P, X>
        where
            Self: HasPlace,
            P: Projection,
            <P as Projection>::Source => <Self as HasPlace>::Target,
        {

            // FIXME: a-mir-formality doesn't support const trait items yet.
            // const SAFETY: bool;

            // FIXME: a-mir-formality doesn't support const trait items yet.
            // const BORROW_KIND: BorrowKind;
            type BorrowDuration: [BorrowDuration];

            unsafe fn borrow(this: *const Self, proj: P) -> X;
        }

        // The duration of the loan tracked by the borrow checker.
        //
        // The borrow checker ensures that `this` has `ACCESS` rights to `P`.
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

        unsafe trait PlaceWrapper<P>
        where
            Self: HasPlace,
            P: Projection,
            <P as Projection>::Source => <Self as HasPlace>::Target,
        {
            type WrappedProjection: [Projection]
            where
                <<Self as PlaceWrapper<P>>::WrappedProjection as Projection>::Source => Self;

            fn wrap_projection(proj: P) -> <Self as PlaceWrapper<P>>::WrappedProjection;
        }
        unsafe trait PlaceDeref<P>
        where
            Self: HasPlace,
            P: Projection,
            <P as Projection>::Source => <Self as HasPlace>::Target,
            <P as Projection>::Target: HasPlace,
        {
            unsafe fn deref(ptr: *mut Self, proj: P) -> *const <P as Projection>::Target;
        }
    }
}
