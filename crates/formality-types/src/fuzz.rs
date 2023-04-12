//! Fuzzing code for fuzzing types etc.
//!
//! Must be customized by a `FuzzDecls` trait that gives context, to avoid generating nonsense.

use crate::cast::Upcast;
use crate::collections::Map;
use crate::derive_links::{Parameter, ParameterKind, Variable};
use crate::fold::Fold;
use crate::grammar::{
    fresh_bound_var, AdtId, AliasName, AliasTy, AssociatedItemId, AssociatedTyName, Binder,
    BoundVar, FieldId, Lt, LtData, PredicateTy, RefKind, RigidName, RigidTy, ScalarId, TraitId,
    TraitRef, Ty, VariantId,
};
use std::ops::Range;
use std::rc::Rc;

pub trait FuzzDriver {
    fn gen_usize(&mut self, range: Range<usize>) -> Option<usize>;
}

impl dyn FuzzDriver + '_ {
    pub fn pick<'a, K: Clone + 'a>(
        &mut self,
        iter: impl ExactSizeIterator<Item = &'a K>,
    ) -> Option<K> {
        let len = iter.len();
        assert!(len > 0, "cannot pick from an empty list");
        let index = self.gen_usize(0..len)?;
        Some(iter.skip(index).next().unwrap().clone())
    }
}

pub struct Fuzzer<'f> {
    pub driver: &'f mut dyn FuzzDriver,
    pub traits: Map<TraitId, Vec<ParameterKind>>,
    pub adts: Map<AdtId, Vec<ParameterKind>>,
    pub associated_types: Map<AssociatedItemId, (TraitId, Vec<ParameterKind>)>,
    pub variables: Vec<BoundVar>,
    pub field_ids: Vec<FieldId>,
    pub variant_ids: Vec<VariantId>,
}

impl<'f> Fuzzer<'f> {
    pub fn binder<R>(
        &mut self,
        kinds: &[ParameterKind],
        op: impl FnOnce(&mut Self) -> Option<R>,
    ) -> Option<Binder<R>>
    where
        R: Fold,
    {
        let vars: Vec<BoundVar> = kinds.iter().map(|&kind| fresh_bound_var(kind)).collect();
        let len = self.variables.len();
        self.variables.extend(&vars);
        let result = op(self);
        self.variables.truncate(len);
        let result = result?;
        Some(Binder::new(vars, result))
    }

    fn has_trait_ids(&self) -> bool {
        !self.traits.is_empty()
    }

    fn pick_trait_id(&mut self) -> Option<TraitId> {
        self.driver.pick(self.traits.keys())
    }

    fn parameters_for_trait_id(&mut self, id: &TraitId) -> Option<Vec<Parameter>> {
        let kinds = self.traits[id].clone(); // appease borrow checker
        self.fuzz_parameters(&kinds)
    }

    fn has_adt_ids(&self) -> bool {
        !self.adts.is_empty()
    }

    fn pick_adt_id(&mut self) -> Option<AdtId> {
        assert!(self.has_adt_ids());
        self.driver.pick(self.adts.keys())
    }

    fn parameters_for_adt_id(&mut self, id: &AdtId) -> Option<Vec<Parameter>> {
        let kinds = self.adts[id].clone(); // appease borrow checker
        self.fuzz_parameters(&kinds)
    }

    fn has_associated_types(&self) -> bool {
        !self.associated_types.is_empty()
    }

    fn pick_associated_type_name(&mut self) -> Option<AssociatedTyName> {
        let item_id = self.driver.pick(self.associated_types.keys())?;
        let trait_id = self.associated_types[&item_id].0.clone();
        Some(AssociatedTyName { item_id, trait_id })
    }

    fn parameters_for_associated_type_name(
        &mut self,
        name: &AssociatedTyName,
    ) -> Option<Vec<Parameter>> {
        let kinds = self.associated_types[&name.item_id].1.clone(); // appease borrow checker
        self.fuzz_parameters(&kinds)
    }

    fn has_variables(&self) -> bool {
        !self.variables.is_empty()
    }

    fn pick_variable(&mut self) -> Option<BoundVar> {
        self.driver.pick(self.variables.iter())
    }

    fn fuzz_parameters(&mut self, kinds: &[ParameterKind]) -> Option<Vec<Parameter>> {
        kinds
            .iter()
            .map(|k| -> Option<Parameter> {
                match k {
                    ParameterKind::Ty => Some(Ty::fuzz(self)?.upcast()),
                    ParameterKind::Lt => Some(Lt::fuzz(self)?.upcast()),
                }
            })
            .collect()
    }
}

impl FuzzDriver for Fuzzer<'_> {
    fn gen_usize(&mut self, range: Range<usize>) -> Option<usize> {
        self.driver.gen_usize(range)
    }
}

pub trait Fuzz: Sized {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool;
    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self>;
}

pub struct PickVariant<'d, 'f, T> {
    fuzzer: &'d mut Fuzzer<'f>,
    options: Vec<Rc<dyn Fn(&mut Fuzzer<'f>) -> Option<T> + 'd>>,
}

impl<'d, 'f, T> PickVariant<'d, 'f, T> {
    pub fn new(fuzzer: &'d mut Fuzzer<'f>) -> Self {
        Self {
            fuzzer,
            options: vec![],
        }
    }

    pub fn variant<U: Upcast<T>>(
        mut self,
        inhabited: impl Fn(&Fuzzer<'_>) -> bool + 'd,
        fuzz: impl Fn(&mut Fuzzer<'_>) -> Option<U> + 'd,
    ) -> Self {
        if inhabited(&self.fuzzer) {
            self.options
                .push(Rc::new(move |fuzzer: &mut Fuzzer<'_>| -> Option<T> {
                    Some(fuzz(fuzzer)?.upcast())
                }));
        }
        self
    }

    pub fn finish(self) -> Option<T> {
        let option = self.fuzzer.driver.pick(self.options.iter())?;
        option(self.fuzzer)
    }
}

impl Fuzz for AdtId {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        fuzzer.has_adt_ids()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        fuzzer.pick_adt_id()
    }
}

impl Fuzz for TraitId {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        fuzzer.has_trait_ids()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        fuzzer.pick_trait_id()
    }
}

impl Fuzz for AssociatedTyName {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        fuzzer.has_associated_types()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        fuzzer.pick_associated_type_name()
    }
}

impl Fuzz for TraitRef {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        AdtId::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let id: TraitId = TraitId::fuzz(fuzzer)?;
        let parameters = fuzzer.parameters_for_trait_id(&id)?;
        Some(TraitRef::new(&id, parameters))
    }
}

macro_rules! fuzz_c_like_enum {
    ($n:ty) => {
        impl Fuzz for $n {
            fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
                !Self::variants().is_empty()
            }

            fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
                fuzzer.driver.pick(Self::variants().iter())
            }
        }
    };
}

fuzz_c_like_enum!(ScalarId);
fuzz_c_like_enum!(RefKind);
fuzz_c_like_enum!(ParameterKind);

impl Fuzz for RigidName {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(AdtId::inhabited, AdtId::fuzz)
            .variant(ScalarId::inhabited, ScalarId::fuzz)
            .variant(RefKind::inhabited, RefKind::fuzz)
            .variant(Arity::inhabited, |fuzzer| {
                let arity = Arity::fuzz(fuzzer)?;
                Some(RigidName::Tuple(arity.0))
            })
            .variant(Arity::inhabited, |fuzzer| {
                let arity = Arity::fuzz(fuzzer)?;
                Some(RigidName::FnPtr(arity.0))
            })
            .finish()
    }
}

impl Fuzz for RigidTy {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        RigidName::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let name = RigidName::fuzz(fuzzer)?;
        let parameters = match &name {
            RigidName::AdtId(id) => fuzzer.parameters_for_adt_id(&id)?,
            RigidName::ScalarId(_) => vec![],
            RigidName::Ref(_) => fuzzer.fuzz_parameters(&[ParameterKind::Ty])?,
            RigidName::Tuple(arity) => fuzzer.fuzz_parameters(&vec![ParameterKind::Ty; *arity])?,
            RigidName::FnPtr(num_args) => {
                fuzzer.fuzz_parameters(&vec![ParameterKind::Ty; num_args + 1])?
            }
            RigidName::FnDef(_) => unimplemented!(),
        };
        Some(RigidTy { name, parameters })
    }
}

impl Fuzz for AliasName {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        fuzzer.has_associated_types()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let name = fuzzer.pick_associated_type_name()?;
        Some(name.upcast())
    }
}

impl Fuzz for AliasTy {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        AliasName::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let name = AliasName::fuzz(fuzzer)?;
        let parameters = match &name {
            AliasName::AssociatedTyId(id) => fuzzer.parameters_for_associated_type_name(id)?,
        };
        Some(AliasTy { name, parameters })
    }
}

/// Generally speaking, we limit the "arity" (number of type arguments) of things to keep fuzzing under control.
pub const MAX_ARITY: usize = 3;

pub struct Arity(pub usize);

impl Fuzz for Arity {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let u = fuzzer.gen_usize(0..MAX_ARITY)?;
        Some(Arity(u))
    }
}

pub const MAX_BOUND_PARAMETERS: usize = 5;

impl<T> Fuzz for Binder<T>
where
    T: Fuzz + Fold,
{
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        // FIXME: in theory, we should be checking if something is inhabited *when it has variables*,
        // but we can't easily do that, particularly since we don't know how many variables we'll have yet,
        // and we never fuzz binders on a type where that matters
        assert!(T::inhabited(fuzzer));
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let num_vars = fuzzer.gen_usize(0..MAX_BOUND_PARAMETERS)?;
        let kinds = (0..num_vars)
            .map(|_| ParameterKind::fuzz(fuzzer))
            .collect::<Option<Vec<_>>>()?;
        fuzzer.binder(&kinds, T::fuzz)
    }
}

impl Fuzz for PredicateTy {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        <Binder<Ty>>::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(<Binder<Ty>>::inhabited, |fuzzer| {
                let binder = <Binder<Ty>>::fuzz(fuzzer)?;
                Some(PredicateTy::ForAll(binder))
            })
            .finish()
    }
}

impl Fuzz for Variable {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        fuzzer.has_variables()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        Some(fuzzer.pick_variable()?.upcast())
    }
}

impl Fuzz for Ty {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(RigidTy::inhabited, RigidTy::fuzz)
            .variant(AliasTy::inhabited, AliasTy::fuzz)
            .variant(PredicateTy::inhabited, PredicateTy::fuzz)
            .variant(Variable::inhabited, Variable::fuzz)
            .finish()
    }
}

impl Fuzz for Lt {
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(|_| true, |_| Some(LtData::Static))
            .variant(Variable::inhabited, Variable::fuzz)
            .finish()
    }
}

const MAX_VEC_LEN: usize = 64;

impl<T> Fuzz for Vec<T>
where
    T: Fuzz,
{
    fn inhabited(_fuzzer: &Fuzzer<'_>) -> bool {
        true
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        if T::inhabited(fuzzer) {
            let l = fuzzer.gen_usize(0..MAX_VEC_LEN)?;
            (0..l).map(|_| T::fuzz(fuzzer)).collect()
        } else {
            Some(vec![])
        }
    }
}

impl<A, B> Fuzz for (A, B)
where
    A: Fuzz,
    B: Fuzz,
{
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        A::inhabited(fuzzer) && B::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let a = A::fuzz(fuzzer)?;
        let b = B::fuzz(fuzzer)?;
        Some((a, b))
    }
}

impl<A, B, C> Fuzz for (A, B, C)
where
    A: Fuzz,
    B: Fuzz,
    C: Fuzz,
{
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        A::inhabited(fuzzer) && B::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        let a = A::fuzz(fuzzer)?;
        let b = B::fuzz(fuzzer)?;
        let c = C::fuzz(fuzzer)?;
        Some((a, b, c))
    }
}

impl Fuzz for Parameter {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        Ty::inhabited(fuzzer) || Lt::inhabited(fuzzer)
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        PickVariant::new(fuzzer)
            .variant(Ty::inhabited, Ty::fuzz)
            .variant(Lt::inhabited, Lt::fuzz)
            .finish()
    }
}

impl Fuzz for FieldId {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        !fuzzer.field_ids.is_empty()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        fuzzer.driver.pick(fuzzer.field_ids.iter())
    }
}

impl Fuzz for VariantId {
    fn inhabited(fuzzer: &Fuzzer<'_>) -> bool {
        !fuzzer.variant_ids.is_empty()
    }

    fn fuzz(fuzzer: &mut Fuzzer<'_>) -> Option<Self> {
        fuzzer.driver.pick(fuzzer.variant_ids.iter())
    }
}
