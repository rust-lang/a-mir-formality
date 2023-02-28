use formality_types::{collections::Set, grammar::AtomicRelation};

pub type ConstraintSet = Set<AtomicRelation>;

macro_rules! all {
    () => {
        Wcs::t()
    };

    ($a:expr $(, $b:expr)* $(,)*) => {
        <_ as Upcast<Wcs>>::upcast($a)
        $(.union($b))*
    };
}

mod prove_after;
mod prove_apr;
mod prove_apr_via;
mod prove_eq;
mod prove_wc;
mod prove_wc_list;

pub use prove_wc_list::prove_wc_list;
