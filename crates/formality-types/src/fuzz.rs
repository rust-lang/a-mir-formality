//! Fuzzing code for fuzzing types etc.
//!
//! Must be customized by a `FuzzDecls` trait that gives context, to avoid generating nonsense.

use crate::cast::UpcastFrom;
use crate::derive_links::{Parameter, ParameterKind, Variable};
use crate::fold::Fold;
use crate::grammar::{
    fresh_bound_var, AdtId, AssociatedItemId, Binder, BoundVar, Lt, LtData, RigidName, RigidTy,
    ScalarId, TraitId, TraitRef, Ty, TyData, Wc, WcData,
};
use bolero_generator::{Driver, TypeGenerator};
use std::{cell::RefCell, ops::Bound};

pub trait FuzzDecls {
    fn trait_ids(&self) -> &[TraitId];
    fn kinds_for_trait_id(&self, id: &TraitId) -> &[ParameterKind];

    fn adt_ids(&self) -> &[AdtId];
    fn kinds_for_adt_id(&self, id: &AdtId) -> &[ParameterKind];

    fn associated_item_ids(&self) -> &[AssociatedItemId];
    fn kinds_for_associated_item_id(&self, id: &AssociatedItemId) -> &[ParameterKind];
    fn trait_for_associated_item_id(&self, id: &AssociatedItemId) -> TraitId;
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
    fn trait_ids(&self) -> Vec<TraitId> {
        panic!("no fuzz decls in scope")
    }

    fn kinds_for_trait_id(&self, id: &TraitId) -> &[ParameterKind] {
        panic!("no fuzz decls in scope")
    }

    fn adt_ids(&self) -> &[AdtId] {
        panic!("no fuzz decls in scope")
    }

    fn kinds_for_adt_id(&self, id: &AdtId) -> Vec<ParameterKind> {
        panic!("no fuzz decls in scope")
    }

    fn associated_item_ids(&self) -> Vec<AssociatedItemId> {
        panic!("no fuzz decls in scope")
    }

    fn kinds_for_associated_item_id(&self, id: &AssociatedItemId) -> Vec<ParameterKind> {
        panic!("no fuzz decls in scope")
    }

    fn trait_for_associated_item_id(&self, id: &AssociatedItemId) -> TraitId {
        panic!("no fuzz decls in scope")
    }
}

struct InScopeFuzzDecls;

impl InScopeFuzzDecls {
    fn has_trait_ids(&self) -> bool {
        FUZZ_DECLS.with(|fd| fd.borrow().trait_ids().is_empty())
    }

    fn pick_trait_id(&self, driver: &mut impl Driver) -> Option<TraitId> {
        assert!(self.has_trait_ids());
        FUZZ_DECLS.with(|fd| pick(&fd.borrow().trait_ids(), driver))
    }

    fn kinds_for_trait_id(&self, id: &TraitId) -> Vec<ParameterKind> {
        FUZZ_DECLS.with(|fd| fd.borrow().kinds_for_trait_id(id).to_vec())
    }

    fn has_adt_ids(&self) -> bool {
        FUZZ_DECLS.with(|fd| fd.borrow().adt_ids().is_empty())
    }

    fn pick_adt_id(&self, driver: &mut impl Driver) -> Option<AdtId> {
        assert!(self.has_adt_ids());
        FUZZ_DECLS.with(|fd| pick(&fd.borrow().adt_ids(), driver))
    }

    fn kinds_for_adt_id(&self, id: &AdtId) -> Vec<ParameterKind> {
        FUZZ_DECLS.with(|fd| fd.borrow().kinds_for_adt_id(id).to_vec())
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

/// Pick a string randomly, so long as the associated bool is true.
/// The boolean is used to avoid picking a variant when it's not available -- e.g., if there
/// are no AdtIds to pick from, we don't want to generate an ADT type.
fn pick_variant(v: &[(&'static str, bool)], driver: &mut impl Driver) -> Option<&'static str> {
    let variants: Vec<&str> = v
        .iter()
        .filter_map(|(s, b)| if b { Some(s) } else { None })
        .collect();
    pick(&variants, driver)
}

fn fuzz_parameters<D: Driver>(kinds: &[ParameterKind], driver: &mut D) -> Option<Vec<Parameter>> {
    kinds
        .iter()
        .map(|k| -> Option<Parameter> {
            match k {
                ParameterKind::Ty => Some(driver.gen::<Ty>()?),
                ParameterKind::Lt => Some(driver.gen::<Lt>()?),
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
        let v = pick_variant(
            &[
                ("adt", InScopeFuzzDecls.has_adt_ids()),
                ("scalar", true),
                ("ref", true),
                ("tuple", true),
                ("fnptr", true),
                ("fndef", false), // FIXME
            ],
            driver,
        )?;

        match v {
            "adt" => {
                let adt_id = InScopeFuzzDecls.pick_adt_id(driver)?;
                let kinds = InScopeFuzzDecls.kinds_for_adt_id(&adt_id);
                Some(RigidTy {
                    name: adt_id.upcast(),
                    parameters: fuzz_parameters(&kinds, driver)?,
                })
            }

            "scalar" => Some(driver.gen::<ScalarId>()?.upcast()),

            "ref" => Some(RigidTy {
                name: RigidName::Ref(driver.gen()?),
                parameters: fuzz_parameters(&[ParameterKind::Ty], driver)?,
            }),

            "tuple" => {
                let arity: Arity = driver.gen()?;
                let kinds = vec![ParameterKind::Ty; arity.0];
                Some(RigidTy {
                    name: RigidName::Tuple(arity.0),
                    parameters: fuzz_parameters(&kinds, driver)?,
                })
            }

            "fnptr" => {
                let num_args: Arity = driver.gen()?;
                let kinds = vec![ParameterKind::Ty; num_args.0 + 1];
                Some(RigidTy {
                    name: RigidName::FnPtr(num_args.0),
                    parameters: fuzz_parameters(&kinds, driver)?,
                })
            }

            "fndef" => {
                panic!()
            }

            _ => panic!("unhandled variant"),
        }
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

// --------------------------------------------------------------------------------
// Bound variables

thread_local! {
    static BOUND_VARIABLES_IN_SCOPE: RefCell<Vec<BoundVar>> = RefCell::new(vec![]);
}

fn with_bound_variable_in_scope<R>(e: &[BoundVar], op: impl FnOnce() -> R) -> R {
    BOUND_VARIABLES_IN_SCOPE.with(|v| with(v, e, op))
}

impl TypeGenerator for Variable {
    fn generate<D: bolero_generator::Driver>(driver: &mut D) -> Option<Self> {
        let bv: BoundVar = BOUND_VARIABLES_IN_SCOPE.with(|u| pick(&u.borrow(), driver))?;
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
