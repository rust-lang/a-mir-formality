use super::*;
use crate::cast_impl;

cast_impl!(impl(L: Language) Variable<L>);
cast_impl!(impl(L: Language) BoundVar<L>);
cast_impl!(impl(L: Language) ExistentialVar<L>);
cast_impl!(impl(L: Language) UniversalVar<L>);
cast_impl!(impl(L: Language) Variable(L)::ExistentialVar(ExistentialVar<L>));
cast_impl!(impl(L: Language) Variable(L)::BoundVar(BoundVar<L>));
cast_impl!(impl(L: Language) Variable(L)::UniversalVar(UniversalVar<L>));
