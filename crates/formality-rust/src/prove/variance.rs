//! Variance of the parameters of a type constructor.
//!
//! Subtyping relates the parameters of two applications of the same type
//! constructor, and *how* it relates them depends on where each parameter
//! appears. `&'a T` may be replaced by `&'b T` when `'a: 'b`, but `&'a mut T`
//! may not be replaced by `&'a mut U` for any `U` other than `T`, because the
//! reference can be both read and written through.
//!
//! Builtin constructors have fixed variances; an ADT's are computed from the
//! types of its fields, as a fixed point so that recursive types converge (a
//! parameter that only appears inside itself stays bivariant rather than
//! collapsing to invariant).
//!
//! See [`prove_sub`](crate::prove::prove_sub::prove_sub) for the use, and
//! `rustc_hir_analysis::variance` for the equivalent in the compiler.

use std::cell::RefCell;
use std::sync::Arc;

use formality_core::{Map, Upcast};

use crate::grammar::{
    Adt, AdtId, BoundVar, CrateItem, Crates, Lt, Parameter, ParameterKind, PtrKind, RefKind,
    Relation, RigidName, RigidTy, Ty, Variable, Wcs,
};
use crate::prove::decls::Program;

/// How a parameter of a type constructor may vary between a subtype and its
/// supertype.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Variance {
    /// The parameter does not appear, so anything may be substituted for it.
    /// The identity of [`Variance::join`], and the starting point of the ADT
    /// fixed point.
    Bivariant,

    /// `Foo<A> <: Foo<B>` when `A <: B` -- the ordinary case, e.g. `&'a T`.
    Covariant,

    /// `Foo<A> <: Foo<B>` when `B <: A`, e.g. the argument of a function
    /// pointer.
    Contravariant,

    /// `Foo<A> <: Foo<B>` only when `A == B`, e.g. the referent of `&mut`.
    Invariant,
}

impl Variance {
    /// The variance of a position that is `self` within a context that is
    /// `outer`. Composing covariance leaves a variance alone, contravariance
    /// flips it, invariance absorbs, bivariance annihilates.
    pub fn xform(outer: Variance, inner: Variance) -> Variance {
        match (outer, inner) {
            (Variance::Bivariant, _) | (_, Variance::Bivariant) => Variance::Bivariant,
            (Variance::Invariant, _) | (_, Variance::Invariant) => Variance::Invariant,
            (Variance::Covariant, v) => v,
            (Variance::Contravariant, Variance::Covariant) => Variance::Contravariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Covariant,
        }
    }

    /// The least variance permitting both `self` and `other`. A parameter
    /// appearing both covariantly and contravariantly is invariant.
    pub fn join(self, other: Variance) -> Variance {
        match (self, other) {
            (Variance::Bivariant, v) | (v, Variance::Bivariant) => v,
            (a, b) if a == b => a,
            _ => Variance::Invariant,
        }
    }

    /// Whether a parameter in this position must be equated rather than merely
    /// related in one direction.
    pub fn is_invariant(self) -> bool {
        self == Variance::Invariant
    }
}

/// The variance of each parameter of the type constructor `name`, which is
/// applied to `arity` parameters.
///
/// The `arity` is taken from the actual application rather than the
/// declaration, so a malformed program cannot cause a length mismatch
/// downstream; an ADT whose declaration disagrees falls back to invariant,
/// which is the conservative direction.
pub fn parameter_variances(program: &Program, name: &RigidName, arity: usize) -> Vec<Variance> {
    match name {
        // `&'a T` / `&'a mut T`: the lifetime is covariant either way, the
        // referent only when it cannot be written through.
        RigidName::Ref(RefKind::Shared) => vec![Variance::Covariant, Variance::Covariant],
        RigidName::Ref(RefKind::Mut) => vec![Variance::Covariant, Variance::Invariant],

        // Raw pointers mirror references.
        RigidName::Raw(PtrKind::Const) => vec![Variance::Covariant],
        RigidName::Raw(PtrKind::Mut) => vec![Variance::Invariant],

        RigidName::Tuple(_) => vec![Variance::Covariant; arity],

        // Parameters are the inputs followed by the output, per
        // `to_rust::tys::lower_fn_ptr`.
        RigidName::FnPtr(_) if arity > 0 => {
            let mut v = vec![Variance::Contravariant; arity - 1];
            v.push(Variance::Covariant);
            v
        }

        RigidName::AdtId(adt_id) => match adt_variances(program).get(adt_id) {
            Some(variances) if variances.len() == arity => variances.clone(),
            _ => vec![Variance::Invariant; arity],
        },

        // A `FnDef`'s parameters instantiate the function's generics, whose
        // variance would have to come from the signature. Invariant is the
        // conservative answer and costs nothing today, since a `FnDef` value is
        // only ever called, never related to another `FnDef`.
        RigidName::FnDef(_) => vec![Variance::Invariant; arity],

        RigidName::ScalarId(_) | RigidName::Never | RigidName::FnPtr(_) => {
            vec![Variance::Invariant; arity]
        }
    }
}

/// The cache behind [`adt_variances`]: each entry keeps its `Arc<Crates>`
/// alive so the key address cannot be reused by a later program.
type AdtVarianceCache = Vec<(Arc<Crates>, Arc<Map<AdtId, Vec<Variance>>>)>;

/// Variance of every ADT in the program, computed once per `Crates`.
///
/// Keyed on the identity of the `Arc<Crates>` rather than its contents, and
/// holding that `Arc` alive so the address cannot be reused by a later program.
fn adt_variances(program: &Program) -> Arc<Map<AdtId, Vec<Variance>>> {
    thread_local! {
        static CACHE: RefCell<AdtVarianceCache> = const { RefCell::new(Vec::new()) };
    }

    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some((_, variances)) = cache
            .iter()
            .find(|(crates, _)| Arc::ptr_eq(crates, &program.crates))
        {
            return variances.clone();
        }

        let variances = Arc::new(compute_adt_variances(&program.crates));
        cache.push((program.crates.clone(), variances.clone()));
        variances
    })
}

/// Compute the variance of every ADT parameter as a fixed point.
///
/// Every parameter starts bivariant and is repeatedly re-derived from the field
/// types until nothing changes. Starting at the bottom of the lattice is what
/// lets a recursive type such as `struct List<'a> { next: Option<&'a List<'a>> }`
/// settle on covariant rather than being forced to invariant by its own
/// occurrence.
fn compute_adt_variances(crates: &Crates) -> Map<AdtId, Vec<Variance>> {
    let adts: Vec<Adt> = crates
        .items_from_all_crates()
        .filter_map(|item| match item {
            CrateItem::AdtItem(adt) => Some(adt.to_adt()),
            _ => None,
        })
        .collect();

    let mut variances: Map<AdtId, Vec<Variance>> = adts
        .iter()
        .map(|adt| {
            (
                adt.id.clone(),
                vec![Variance::Bivariant; adt.binder.kinds().len()],
            )
        })
        .collect();

    loop {
        let mut changed = false;

        for adt in &adts {
            let (vars, bound) = adt.binder.open();
            let updated: Vec<Variance> = vars
                .iter()
                .map(|var| {
                    bound
                        .variants
                        .iter()
                        .flat_map(|variant| &variant.fields)
                        .fold(Variance::Bivariant, |acc, field| {
                            acc.join(variance_of_var_in_parameter(
                                &variances,
                                var,
                                &field.ty.clone().upcast(),
                                Variance::Covariant,
                            ))
                        })
                })
                .collect();

            if variances[&adt.id] != updated {
                variances.insert(adt.id.clone(), updated);
                changed = true;
            }
        }

        if !changed {
            return variances;
        }
    }
}

/// The variance of `var` within `parameter`, which itself sits in a position of
/// variance `position`.
fn variance_of_var_in_parameter(
    adt_variances: &Map<AdtId, Vec<Variance>>,
    var: &BoundVar,
    parameter: &Parameter,
    position: Variance,
) -> Variance {
    let target: Parameter = match var.kind {
        ParameterKind::Lt => Lt::Variable(Variable::BoundVar(*var)).upcast(),
        _ => Ty::Variable(Variable::BoundVar(*var)).upcast(),
    };
    variance_of_target_in_parameter(adt_variances, &target, parameter, position)
}

/// How the region `lt` is used within `ty`.
///
/// This is formality's counterpart to rustc's `live_region_variances`, which
/// decides whether the localized constraint graph gets backward (time-
/// travelling) edges for a region: it does exactly when the region is not used
/// covariantly. See `outlives_visible_to_loan` in the borrow checker.
pub fn variance_of_lifetime_in_ty(program: &Program, lt: &Lt, ty: &Ty) -> Variance {
    variance_of_target_in_parameter(
        &adt_variances(program),
        &lt.clone().upcast(),
        &ty.clone().upcast(),
        Variance::Covariant,
    )
}

/// The variance of the occurrences of `target` within `parameter`, which itself
/// sits in a position of variance `position`.
fn variance_of_target_in_parameter(
    adt_variances: &Map<AdtId, Vec<Variance>>,
    target: &Parameter,
    parameter: &Parameter,
    position: Variance,
) -> Variance {
    if parameter == target {
        return position;
    }

    match parameter {
        Parameter::Lt(_) => Variance::Bivariant,

        Parameter::Ty(ty) => match ty.as_ref() {
            Ty::Variable(_) => Variance::Bivariant,

            Ty::RigidTy(RigidTy { name, parameters }) => {
                let inner = rigid_variances(adt_variances, name, parameters.len());
                parameters
                    .iter()
                    .zip(inner)
                    .fold(Variance::Bivariant, |acc, (p, v)| {
                        acc.join(variance_of_target_in_parameter(
                            adt_variances,
                            target,
                            p,
                            Variance::xform(position, v),
                        ))
                    })
            }

            // We cannot see through an alias or a `for<..>` type to tell where
            // the variable ends up, so any occurrence has to be invariant.
            Ty::AliasTy(_) | Ty::PredicateTy(_) => occurs_invariantly(target, parameter),
        },

        Parameter::Const(_) => occurs_invariantly(target, parameter),
    }
}

/// `Invariant` if `target` occurs anywhere in `parameter`, else `Bivariant`.
fn occurs_invariantly(target: &Parameter, parameter: &Parameter) -> Variance {
    if occurs_in(target, parameter) {
        Variance::Invariant
    } else {
        Variance::Bivariant
    }
}

/// Whether `target` appears anywhere inside `parameter`.
fn occurs_in(target: &Parameter, parameter: &Parameter) -> bool {
    if parameter == target {
        return true;
    }
    match parameter {
        Parameter::Lt(_) => false,
        Parameter::Ty(ty) => match ty.as_ref() {
            Ty::Variable(_) => false,
            Ty::RigidTy(RigidTy { parameters, .. }) => {
                parameters.iter().any(|p| occurs_in(target, p))
            }
            Ty::AliasTy(alias) => alias.parameters.iter().any(|p| occurs_in(target, p)),
            Ty::PredicateTy(_) => {
                // Conservative: treat an opaque `for<..>` type as containing it.
                true
            }
        },
        Parameter::Const(_) => false,
    }
}

/// Like [`parameter_variances`], but reading ADT variances from the in-progress
/// fixed-point map rather than the cache.
fn rigid_variances(
    adt_variances: &Map<AdtId, Vec<Variance>>,
    name: &RigidName,
    arity: usize,
) -> Vec<Variance> {
    match name {
        RigidName::AdtId(adt_id) => match adt_variances.get(adt_id) {
            Some(variances) if variances.len() == arity => variances.clone(),
            _ => vec![Variance::Invariant; arity],
        },
        _ => builtin_variances(name, arity),
    }
}

/// The non-ADT cases of [`parameter_variances`], which need no program.
fn builtin_variances(name: &RigidName, arity: usize) -> Vec<Variance> {
    match name {
        RigidName::Ref(RefKind::Shared) => vec![Variance::Covariant, Variance::Covariant],
        RigidName::Ref(RefKind::Mut) => vec![Variance::Covariant, Variance::Invariant],
        RigidName::Raw(PtrKind::Const) => vec![Variance::Covariant],
        RigidName::Raw(PtrKind::Mut) => vec![Variance::Invariant],
        RigidName::Tuple(_) => vec![Variance::Covariant; arity],
        RigidName::FnPtr(_) if arity > 0 => {
            let mut v = vec![Variance::Contravariant; arity - 1];
            v.push(Variance::Covariant);
            v
        }
        _ => vec![Variance::Invariant; arity],
    }
}

/// The goals relating the parameters of two applications of `name`, according
/// to each parameter's variance.
///
/// Covariant parameters are related left-to-right, contravariant ones
/// right-to-left, and invariant ones in both directions -- for a lifetime that
/// is the pair of outlives constraints `'a: 'b` and `'b: 'a`, which is what
/// makes an invariant region equated rather than merely outlived. Bivariant
/// parameters are unconstrained.
pub fn sub_goals_with_variance(
    program: &Program,
    name: &RigidName,
    a_parameters: &[Parameter],
    b_parameters: &[Parameter],
) -> Wcs {
    assert_eq!(a_parameters.len(), b_parameters.len());

    let variances = parameter_variances(program, name, a_parameters.len());

    a_parameters
        .iter()
        .zip(b_parameters)
        .zip(variances)
        .flat_map(|((a, b), variance)| match variance {
            // Identical parameters are related in every direction already.
            // Worth checking here rather than leaving it to `prove_sub`'s
            // `trivial` rule: an invariant position otherwise costs two full
            // proofs, and for a concrete referent like the `Map` in
            // `&'a mut Map` both of them are pure overhead. Skipping them takes
            // `min_problem_case_3` from 150s back to 5s.
            _ if a == b => vec![],

            Variance::Covariant => vec![Relation::sub(a.clone(), b.clone())],
            Variance::Contravariant => vec![Relation::sub(b.clone(), a.clone())],
            // Descending through matching constructors instead, so that this
            // bottoms out at cheap outlives goals, was tried and measured no
            // faster (553s vs 549s over the borrowck suite). The cost is
            // elsewhere.
            Variance::Invariant => vec![
                Relation::sub(a.clone(), b.clone()),
                Relation::sub(b.clone(), a.clone()),
            ],
            Variance::Bivariant => vec![],
        })
        .collect()
}
