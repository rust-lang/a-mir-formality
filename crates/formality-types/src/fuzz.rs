//! Fuzzing code for fuzzing types etc.
//!
//! Must be customized by a `FuzzDecls` trait that gives context, to avoid generating nonsense.

use crate::cast::{Upcast, UpcastFrom};
use crate::derive_links::{Parameter, ParameterKind, Variable};
use crate::fold::Fold;
use crate::grammar::{
    fresh_bound_var, AdtId, AliasTy, AssociatedItemId, AssociatedTyName, Binder, BoundVar, Lt,
    LtData, Predicate, PredicateTy, Relation, RigidName, RigidTy, ScalarId, TraitId, TraitRef, Ty,
    TyData, Wc, WcData, PR,
};
use bolero_generator::{Driver, TypeGenerator};
use std::{cell::RefCell, ops::Bound};

pub trait FuzzDecls {
    fn trait_ids(&self) -> &[TraitId];
    fn kinds_for_trait_id(&self, id: &TraitId) -> &[ParameterKind];

    fn adt_ids(&self) -> &[AdtId];
    fn kinds_for_adt_id(&self, id: &AdtId) -> &[ParameterKind];

    fn associated_type_ids(&self) -> &[AssociatedItemId];
    fn kinds_for_associated_type_id(&self, id: &AssociatedItemId) -> &[ParameterKind];
    fn trait_for_associated_type_id(&self, id: &AssociatedItemId) -> TraitId;
}

thread_local! {
    static FUZZ_DECLS: RefCell<Box<dyn FuzzDecls>> = RefCell::new(Box::new(NoFuzzDecls))
}

pub fn with_fuzz_decls<R>(d: impl FuzzDecls + 'static, op: impl FnOnce() -> R) -> R {
    FUZZ_DECLS.with(|fd| {
        let old_decls = std::mem::replace(&mut *fd.borrow_mut(), Box::new(d));
        let result = op();
        *fd.borrow_mut() = old_decls;
        result
    })
}

struct NoFuzzDecls;

impl FuzzDecls for NoFuzzDecls {
    fn trait_ids(&self) -> &[TraitId] {
        panic!("no fuzz decls in scope")
    }

    fn kinds_for_trait_id(&self, _id: &TraitId) -> &[ParameterKind] {
        panic!("no fuzz decls in scope")
    }

    fn adt_ids(&self) -> &[AdtId] {
        panic!("no fuzz decls in scope")
    }

    fn kinds_for_adt_id(&self, _id: &AdtId) -> &[ParameterKind] {
        panic!("no fuzz decls in scope")
    }

    fn associated_type_ids(&self) -> &[AssociatedItemId] {
        panic!("no fuzz decls in scope")
    }

    fn kinds_for_associated_type_id(&self, _id: &AssociatedItemId) -> &[ParameterKind] {
        panic!("no fuzz decls in scope")
    }

    fn trait_for_associated_type_id(&self, _id: &AssociatedItemId) -> TraitId {
        panic!("no fuzz decls in scope")
    }
}

// --------------------------------------------------------------------------------
// Bound variables

thread_local! {
    static BOUND_VARIABLES_IN_SCOPE: RefCell<Vec<BoundVar>> = RefCell::new(vec![]);
}

fn with_bound_variable_in_scope<R>(e: &[BoundVar], op: impl FnOnce() -> R) -> R {
    BOUND_VARIABLES_IN_SCOPE.with(|v| with(v, e, op))
}

struct InScopeFuzzDecls;

impl InScopeFuzzDecls {
    fn has_trait_ids(&self) -> bool {
        FUZZ_DECLS.with(|fd| !fd.borrow().trait_ids().is_empty())
    }

    fn pick_trait_id(&self, driver: &mut impl Driver) -> Option<TraitId> {
        assert!(self.has_trait_ids());
        FUZZ_DECLS.with(|fd| pick(&fd.borrow().trait_ids(), driver))
    }

    fn kinds_for_trait_id(&self, id: &TraitId) -> Vec<ParameterKind> {
        FUZZ_DECLS.with(|fd| fd.borrow().kinds_for_trait_id(id).to_vec())
    }

    fn has_adt_ids(&self) -> bool {
        FUZZ_DECLS.with(|fd| !fd.borrow().adt_ids().is_empty())
    }

    fn pick_adt_id(&self, driver: &mut impl Driver) -> Option<AdtId> {
        assert!(self.has_adt_ids());
        FUZZ_DECLS.with(|fd| pick(&fd.borrow().adt_ids(), driver))
    }

    fn kinds_for_adt_id(&self, id: &AdtId) -> Vec<ParameterKind> {
        FUZZ_DECLS.with(|fd| fd.borrow().kinds_for_adt_id(id).to_vec())
    }

    fn has_associated_types(&self) -> bool {
        FUZZ_DECLS.with(|fd| !fd.borrow().associated_type_ids().is_empty())
    }

    fn pick_associated_type_name(&self, driver: &mut impl Driver) -> Option<AssociatedTyName> {
        FUZZ_DECLS.with(|fd| {
            let item_id = pick(fd.borrow().associated_type_ids(), driver)?;
            let trait_id = fd.borrow().trait_for_associated_type_id(&item_id);
            Some(AssociatedTyName { item_id, trait_id })
        })
    }

    fn kinds_for_associated_type_name(&self, name: &AssociatedTyName) -> Vec<ParameterKind> {
        FUZZ_DECLS.with(|fd| {
            fd.borrow()
                .kinds_for_associated_type_id(&name.item_id)
                .to_vec()
        })
    }

    fn has_variables(&self) -> bool {
        BOUND_VARIABLES_IN_SCOPE.with(|v| !v.borrow().is_empty())
    }

    fn pick_variable(&self, driver: &mut impl Driver) -> Option<BoundVar> {
        BOUND_VARIABLES_IN_SCOPE.with(|v| pick(&v.borrow(), driver))
    }
}

fn with<T, R>(v: &RefCell<Vec<T>>, e: &[T], op: impl FnOnce() -> R) -> R
where
    T: Copy,
{
    let len = v.borrow().len();
    v.borrow_mut().extend(e);
    let result = op();
    v.borrow_mut().truncate(len);
    result
}

/// Fuzz by picking an item randomly out of `v`, returning `None` if `v` is empty.
fn pick<T>(v: &[T], driver: &mut impl Driver) -> Option<T>
where
    T: Clone,
{
    let l = v.len();
    if l == 0 {
        return None;
    }
    let index = driver.gen_usize(Bound::Included(&0), Bound::Excluded(&l))?;
    Some(v[index].clone())
}

struct PickVariant<'d, T, D> {
    driver: &'d mut D,
    v: Vec<Box<dyn FnOnce(&mut D) -> Option<T>>>,
}

impl<'d, T, D> PickVariant<'d, T, D>
where
    D: Driver,
{
    pub fn new(driver: &'d mut D) -> Self {
        Self { driver, v: vec![] }
    }

    pub fn variant<U>(mut self, cond: bool, f: impl FnOnce(&mut D) -> Option<U> + 'static) -> Self
    where
        U: Upcast<T>,
    {
        if cond {
            self.v
                .push(Box::new(move |driver| Some(f(driver)?.upcast())));
        }
        self
    }

    pub fn finish(mut self) -> Option<T> {
        assert!(self.v.len() > 0);
        let l = self.v.len();
        let index = self
            .driver
            .gen_usize(Bound::Included(&0), Bound::Excluded(&l))?;
        let f = self.v.remove(index);
        f(self.driver)
    }
}

fn fuzz_parameters<D: Driver>(kinds: &[ParameterKind], driver: &mut D) -> Option<Vec<Parameter>> {
    kinds
        .iter()
        .map(|k| -> Option<Parameter> {
            match k {
                ParameterKind::Ty => Some(driver.gen::<Ty>()?.upcast()),
                ParameterKind::Lt => Some(driver.gen::<Lt>()?.upcast()),
            }
        })
        .collect()
}

impl TypeGenerator for TraitRef {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let id: TraitId = InScopeFuzzDecls.pick_trait_id(driver)?;
        let kinds = InScopeFuzzDecls.kinds_for_trait_id(&id);
        let parameters = fuzz_parameters(&kinds, driver)?;
        Some(TraitRef::new(&id, parameters))
    }
}

impl TypeGenerator for RigidTy {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        PickVariant::new(driver)
            .variant(InScopeFuzzDecls.has_adt_ids(), |driver| {
                let adt_id = InScopeFuzzDecls.pick_adt_id(driver)?;
                let kinds = InScopeFuzzDecls.kinds_for_adt_id(&adt_id);
                Some(RigidTy {
                    name: adt_id.upcast(),
                    parameters: fuzz_parameters(&kinds, driver)?,
                })
            })
            .variant(true, |driver| Some(driver.gen::<ScalarId>()?))
            .variant(true, |driver| {
                Some(RigidTy {
                    name: RigidName::Ref(driver.gen()?),
                    parameters: fuzz_parameters(&[ParameterKind::Ty], driver)?,
                })
            })
            .variant(true, |driver| {
                let arity: Arity = driver.gen()?;
                let kinds = vec![ParameterKind::Ty; arity.0];
                Some(RigidTy {
                    name: RigidName::Tuple(arity.0),
                    parameters: fuzz_parameters(&kinds, driver)?,
                })
            })
            .variant(true, |driver| {
                let num_args: Arity = driver.gen()?;
                let kinds = vec![ParameterKind::Ty; num_args.0 + 1];
                Some(RigidTy {
                    name: RigidName::FnPtr(num_args.0),
                    parameters: fuzz_parameters(&kinds, driver)?,
                })
            })
            .finish()
    }
}

impl TypeGenerator for AliasTy {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        PickVariant::new(driver)
            .variant(InScopeFuzzDecls.has_associated_types(), |driver| {
                let name: AssociatedTyName = InScopeFuzzDecls.pick_associated_type_name(driver)?;
                let kinds = InScopeFuzzDecls.kinds_for_associated_type_name(&name);
                Some(AliasTy {
                    name: name.upcast(),
                    parameters: fuzz_parameters(&kinds, driver)?,
                })
            })
            .finish()
    }
}

// --------------------------------------------------------------------------------
// Cyclic types
//
// The TypeGenerator derive can't handle these.

impl TypeGenerator for Ty {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let data: TyData = driver.gen()?;
        Some(Ty::upcast_from(data))
    }
}

impl TypeGenerator for TyData {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        PickVariant::new(driver)
            .variant(true, |driver| driver.gen::<RigidTy>())
            .variant(InScopeFuzzDecls.has_variables(), |driver| {
                driver.gen::<Variable>()
            })
            .variant(InScopeFuzzDecls.has_associated_types(), |driver| {
                driver.gen::<AliasTy>()
            })
            .variant(true, |driver| driver.gen::<PredicateTy>())
            .finish()
    }
}

impl TypeGenerator for Lt {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let data: LtData = driver.gen()?;
        Some(Lt::upcast_from(data))
    }
}

impl TypeGenerator for Wc {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        let data: WcData = driver.gen()?;
        Some(Wc::upcast_from(data))
    }
}

impl TypeGenerator for Variable {
    fn generate<D: bolero_generator::Driver>(driver: &mut D) -> Option<Self> {
        let bv: BoundVar = InScopeFuzzDecls.pick_variable(driver)?;
        Some(bv.upcast())
    }
}

impl<T> TypeGenerator for Binder<T>
where
    T: TypeGenerator + Fold,
{
    fn generate<D: bolero_generator::Driver>(driver: &mut D) -> Option<Self> {
        let kinds: Vec<ParameterKind> = driver.gen()?;
        let bound_vars: Vec<BoundVar> = kinds.iter().map(|&k| fresh_bound_var(k)).collect();
        let data = with_bound_variable_in_scope(&bound_vars, || driver.gen::<T>())?;
        Some(Binder::new(bound_vars, data))
    }
}

impl TypeGenerator for PR {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        PickVariant::new(driver)
            .variant(InScopeFuzzDecls.has_trait_ids(), |driver| {
                driver.gen::<Predicate>()
            })
            .variant(true, |driver| driver.gen::<Relation>())
            .finish()
    }
}

// --------------------------------------------------------------------------------

/// Generally speaking, we limit the "arity" (number of type arguments) of things to keep fuzzing under control.
pub const MAX_ARITY: usize = 3;

pub struct Arity(pub usize);

impl TypeGenerator for Arity {
    fn generate<D: Driver>(driver: &mut D) -> Option<Self> {
        Some(Arity(driver.gen_usize(
            Bound::Included(&0),
            Bound::Excluded(&MAX_ARITY),
        )?))
    }
}
