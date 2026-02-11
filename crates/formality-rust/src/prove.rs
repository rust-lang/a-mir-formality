use crate::grammar::{
    Program, WhereBound, WhereBoundData, WhereClause, WhereClauseData,
};
use formality_core::Upcast;
pub mod prove;
use crate::grammar::{Predicate, Relation, Ty, Wc, Wcs};

impl Program {
    pub fn to_prove_decls(&self) -> prove::Decls {
        prove::Decls {
            program: self.clone(),
            max_size: prove::Decls::DEFAULT_MAX_SIZE,
        }
    }
}

pub trait ToWcs {
    fn to_wcs(&self) -> Wcs;
}

impl<T: ?Sized + ToWcs> ToWcs for &T {
    fn to_wcs(&self) -> Wcs {
        T::to_wcs(self)
    }
}

macro_rules! upcast_to_wcs {
    ($($t:ty,)*) => {
        $(
            impl ToWcs for $t {
                fn to_wcs(&self) -> Wcs {
                    self.upcast()
                }
            }
        )*
    }
}

upcast_to_wcs! {
    Wc,
    Wcs,
    Predicate,
    Relation,
}

impl ToWcs for () {
    fn to_wcs(&self) -> Wcs {
        Wcs::t()
    }
}

impl<A, B> ToWcs for (A, B)
where
    A: ToWcs,
    B: ToWcs,
{
    fn to_wcs(&self) -> Wcs {
        let (a, b) = self;
        let a = a.to_wcs();
        let b = b.to_wcs();
        (a, b).upcast()
    }
}

impl<A, B, C> ToWcs for (A, B, C)
where
    A: ToWcs,
    B: ToWcs,
    C: ToWcs,
{
    fn to_wcs(&self) -> Wcs {
        let (a, b, c) = self;
        let a = a.to_wcs();
        let b = b.to_wcs();
        let c = c.to_wcs();
        (a, b, c).upcast()
    }
}
impl ToWcs for Vec<WhereClause> {
    fn to_wcs(&self) -> Wcs {
        self.iter().flat_map(|wc| wc.to_wcs()).collect()
    }
}

impl ToWcs for [WhereClause] {
    fn to_wcs(&self) -> Wcs {
        self.iter().flat_map(|wc| wc.to_wcs()).collect()
    }
}

impl ToWcs for WhereClause {
    fn to_wcs(&self) -> Wcs {
        match self.data() {
            WhereClauseData::IsImplemented(self_ty, trait_id, parameters) => {
                trait_id.with(self_ty, parameters).upcast()
            }
            WhereClauseData::AliasEq(alias_ty, ty) => {
                Predicate::AliasEq(alias_ty.clone(), ty.clone()).upcast()
            }
            WhereClauseData::Outlives(a, b) => Relation::outlives(a, b).upcast(),
            WhereClauseData::ForAll(binder) => {
                let (vars, wc) = binder.open();
                wc.to_wcs()
                    .into_iter()
                    .map(|wc| Wc::for_all(&vars, wc))
                    .collect()
            }
            WhereClauseData::TypeOfConst(ct, ty) => {
                Predicate::ConstHasType(ct.clone(), ty.clone()).upcast()
            }
        }
    }
}

impl WhereBound {
    pub fn to_wc(&self, self_ty: impl Upcast<Ty>) -> Wc {
        let self_ty: Ty = self_ty.upcast();

        match self.data() {
            WhereBoundData::IsImplemented(trait_id, parameters) => {
                trait_id.with(self_ty, parameters).upcast()
            }
            WhereBoundData::Outlives(lt) => Relation::outlives(self_ty, lt).upcast(),
            WhereBoundData::ForAll(binder) => {
                let (vars, bound) = binder.open();
                Wc::for_all(&vars, bound.to_wc(self_ty))
            }
        }
    }
}
