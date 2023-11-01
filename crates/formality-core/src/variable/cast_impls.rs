use super::*;
use crate::cast_impl;

cast_impl!(impl(L: Language) CoreVariable<L>);
cast_impl!(impl(L: Language) CoreBoundVar<L>);
cast_impl!(impl(L: Language) CoreExistentialVar<L>);
cast_impl!(impl(L: Language) CoreUniversalVar<L>);
cast_impl!(impl(L: Language) CoreVariable(L)::ExistentialVar(CoreExistentialVar<L>));
cast_impl!(impl(L: Language) CoreVariable(L)::BoundVar(CoreBoundVar<L>));
cast_impl!(impl(L: Language) CoreVariable(L)::UniversalVar(CoreUniversalVar<L>));
