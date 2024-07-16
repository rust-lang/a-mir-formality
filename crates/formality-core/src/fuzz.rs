//! See the Formality Book [chapter on fuzzing][f] for more details.
//!
//! [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html

use std::any::{Any, TypeId};
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::Arc;

use bolero::ValueGenerator as _;

use crate::binder::fresh_bound_var;
use crate::language::Language;
use crate::variable::CoreVariable;
use crate::{Map, Set, Upcast, Upcasted};

/// Underlying fuzz driver used to drive the rest of fuzzing
pub trait FuzzDriver<'d>: 'd {
    fn fuzz_u32(&mut self, range: std::ops::Range<u32>) -> Option<u32>;
    fn fuzz_usize(&mut self, range: std::ops::Range<usize>) -> Option<usize>;
    fn fuzz_f64(&mut self, range: std::ops::Range<f64>) -> Option<f64>;
}

/// Configuration for the fuzzer.
#[derive(Clone)]
pub struct FuzzConfig<L: Language> {
    /// Maximum recursion depth for any given type
    max_depth: u32,

    /// Variables introduced by binders earlier on the stack.
    free_variables: Vec<CoreVariable<L>>,

    /// Valid range for u32/usize integer literals
    literal_range: std::ops::Range<u32>,

    /// Range of elements to create for a generic collection (vector, map, etc)
    collection_range: std::ops::Range<usize>,

    /// Range of elements to create for a generic collection (vector, map, etc)
    binder_range: std::ops::Range<usize>,

    /// Stores a range of possible values for the type with the given type-id.
    /// The `Any` will always be a `Vec<T>`.
    valid_values: Map<TypeId, Arc<dyn Any>>,

    /// Stores a map for the given type-ids.
    /// The `Any` will always be a `Map<K, V>`.
    key_values: Map<(TypeId, TypeId), Arc<dyn Any>>,
}

impl<L: Language> FuzzConfig<L> {
    /// Create a default fuzzing configuration
    pub fn new() -> Self {
        Self {
            max_depth: 2,
            free_variables: Default::default(),
            literal_range: 0..22,
            collection_range: 0..3,
            binder_range: 0..2,
            valid_values: Default::default(),
            key_values: Default::default(),
        }
    }

    /// Convert the configuration into a `FuzzCx` that can be used to drive fuzzing.
    /// Must provide a `driver`, typically a `&mut bolero::Driver` is used.
    pub fn into_cx<'d>(self, driver: impl FuzzDriver<'d>) -> FuzzCx<'d, L> {
        FuzzCx {
            depth: Map::new(),
            weights: Map::new(),
            driver: Box::new(driver),
            in_binders: 0,
            free_variables: self.free_variables.clone(),
            config: self,
        }
    }

    /// Configure maximum depth.
    pub fn with_max_depth(mut self, max_depth: u32) -> Self {
        self.max_depth = max_depth;
        self
    }

    /// Configure range of collection length.
    pub fn with_collection_range(mut self, collection_range: std::ops::Range<usize>) -> Self {
        self.collection_range = collection_range;
        self
    }

    /// Configure number of variables to consider for binders.
    pub fn with_binder_range(mut self, binder_range: std::ops::Range<usize>) -> Self {
        self.binder_range = binder_range;
        self
    }

    /// Lookup the value associated with the given value of type `K`.
    /// If no map has been set for this (K,V) type, or if the key is not
    /// found in the map that was set, returns `None`.
    pub fn with_key_values<K: 'static + Ord, V: 'static + Clone>(mut self, map: Map<K, V>) -> Self {
        let key_id = TypeId::of::<K>();
        let value_id = TypeId::of::<V>();
        let old_value = self
            .key_values
            .insert((key_id, value_id), Arc::new(map) as Arc<dyn Any>);

        assert!(
            old_value.is_none(),
            "already set the key-values for `{} -> {}`",
            std::any::type_name::<K>(),
            std::any::type_name::<V>(),
        );

        self
    }

    /// Set the values for the given type `T`.
    /// When fuzzing, the impl can invoke [`Guard::pick_value`][]
    /// to select from these values.
    pub fn with_values<T: 'static>(mut self, values: Vec<T>) -> Self {
        let id = TypeId::of::<T>();
        let old_value = self
            .valid_values
            .insert(id, Arc::new(values) as Arc<dyn Any>);
        assert!(
            old_value.is_none(),
            "already set the values for `{}`",
            std::any::type_name::<T>()
        );
        self
    }

    /// Configure base set of free variables to be used when fuzzing;
    /// more variables may be added if you fuzz a `Binder<T>`.
    ///
    /// # Panics
    ///
    /// If you have already configured the free variable set.
    pub fn with_free_variables(mut self, variables: Vec<impl Upcast<CoreVariable<L>>>) -> Self {
        assert!(self.free_variables.is_empty(), "free variables already set");
        self.free_variables = variables.into_iter().upcasted().collect();
        self
    }

    /// Range of collection length
    pub fn collection_range(&self) -> std::ops::Range<usize> {
        self.collection_range.clone()
    }

    /// Range for integer literals
    pub fn literal_range(&self) -> std::ops::Range<u32> {
        self.literal_range.clone()
    }

    /// Number of variables to consider for binders.
    pub fn binder_range(&self) -> std::ops::Range<usize> {
        self.binder_range.clone()
    }

    /// Returns a reference to the value of the given type `T`.
    /// If the type has not been set, then `None` is returned.
    pub fn get<T: 'static>(&self) -> Option<&T> {
        let id = TypeId::of::<T>();
        let value = self.valid_values.get(&id)?;
        value.downcast_ref::<T>()
    }

    /// Lookup the value associated with the given value of type `K`.
    /// If no map has been set for this (K,V) type, or if the key is not
    /// found in the map that was set, returns `None`.
    #[track_caller]
    pub fn key_value<K: 'static + Ord, V: 'static + Clone>(&self, key: &K) -> Option<V> {
        let key_id = TypeId::of::<K>();
        let value_id = TypeId::of::<V>();
        let Some(map) = self.key_values.get(&(key_id, value_id)) else {
            panic!(
                "no key/value map set for (`{} -> {}`)",
                std::any::type_name::<K>(),
                std::any::type_name::<V>(),
            )
        };
        let map = map.downcast_ref::<Map<K, V>>().unwrap();
        map.get(key).cloned()
    }
}

/// Fuzzing context, created from a [`FuzzConfig`][].
pub struct FuzzCx<'d, L: Language> {
    config: FuzzConfig<L>,

    /// Current recursion depth for any given type.
    depth: Map<TypeId, u32>,

    /// Cached weights for a given type.
    weights: Map<(TypeId, Depth), f64>,

    /// Number of binders we are currently inside.
    /// Incremented and decremented by `FuzzCx::enter`
    /// when entering a type for which `Fuzzable::is_binder`
    /// returns true.
    in_binders: u32,

    /// Variables introduced by binders earlier on the stack.
    free_variables: Vec<CoreVariable<L>>,

    /// Underlying bolero driver.
    driver: Box<dyn FuzzDriver<'d>>,
}

impl<L: Language> Deref for FuzzCx<'_, L> {
    type Target = FuzzConfig<L>;

    fn deref(&self) -> &Self::Target {
        &self.config
    }
}

/// The "depth" at which a type appears.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Depth {
    /// Number of times this type appears on the stack.
    pub depth: u32,

    /// Number of binders opened on the stack.
    pub binders: u32,
}

impl<'d, L: Language> FuzzCx<'d, L> {
    /// Fuzz a value of type `T`.
    ///
    /// Not meant to be used from inside a `Fuzzable` impl; in that context, you should prefer
    /// to invoke [`enter_fuzzable`] and then invoke `fuzz` on the guard you are given.
    pub fn fuzz<T>(&mut self) -> Option<T>
    where
        T: Fuzzable<L>,
    {
        assert!(
            self.depth.is_empty(),
            "`FuzzCx::fuzz` called from inside a `Fuzzable` impl; use `guard.fuzz` instead"
        );
        T::fuzz(self)
    }

    /// Fuzz a vector of the given length.
    pub fn fuzz_many<T, C>(&mut self, range: std::ops::Range<usize>) -> Option<C>
    where
        T: Fuzzable<L>,
        C: FromIterator<T>,
    {
        let len = self.driver.fuzz_usize(range)?;
        (0..len).map(|_| T::fuzz(self)).collect()
    }

    /// Attempts to enter the given type `T` at the given depth.
    /// Returns `None` if the recursion depth is exceeded.
    /// Otherwise, returns a `Some(g)` with a guard that should be dropped when you have finished.
    fn enter<T: Fuzzable<L>, R>(
        &mut self,
        op: impl FnOnce(&mut EnterGuard<'_, 'd, L, T>) -> R,
    ) -> Option<R> {
        let id = TypeId::of::<T>();

        let depth = self.depth.entry(id).or_insert(0);
        if *depth >= self.config.max_depth {
            return None;
        }

        // Track the depth -- number of times we have entered this type.
        // This is used to limit the depth of fuzzing we will do.
        *depth += 1;
        let depth = *depth;

        // Track the number of binders we have entered.
        // This is used to estimate how many free variables we will have at any given point.
        if T::is_binder() {
            self.in_binders += 1;
        }

        let binders = self.in_binders;
        let result = op(&mut EnterGuard {
            cx: self,
            depth: Depth { depth, binders },
            phantom: PhantomData::<T>,
        });

        // Restore binder track
        if T::is_binder() {
            self.in_binders -= 1;
        }

        if depth == 1 {
            self.depth.remove(&id);
        } else {
            *self.depth.get_mut(&id).unwrap() = depth - 1;
        }

        Some(result)
    }

    /// Begin computing the "estimate cardinarily" result
    /// for the type `T`. The returned value will be cached.
    /// Tracks depth and returns 0 if depth would be exceeded.
    pub fn enter_estimate_cardinality<T: Fuzzable<L>>(
        &mut self,
        op: impl FnOnce(&mut EnterGuard<'_, 'd, L, T>) -> f64,
    ) -> f64 {
        self.enter::<T, f64>(|guard| {
            let id = TypeId::of::<T>();
            let depth = guard.depth;
            if let Some(&v) = guard.cx.weights.get(&(id, depth)) {
                return v;
            }

            let result = op(guard);
            guard.cx.weights.insert((id, depth), result);
            result
        })
        .unwrap_or(0_f64)
    }

    /// Attempts to enter the given type `T` at the given depth.
    /// Returns `None` if the recursion depth is exceeded.
    /// Otherwise, returns a `Some(g)` with a guard that should be dropped when you have finished.
    pub fn enter_fuzz<T: Fuzzable<L>>(
        &mut self,
        op: impl FnOnce(&mut EnterGuard<'_, 'd, L, T>) -> Option<T>,
    ) -> Option<T> {
        self.enter(op).unwrap_or(None)
    }

    /// Pick how many instances to create from `range`, given that each element has the given cardinality
    fn pick_count(&mut self, range: std::ops::Range<usize>, cardinality: f64) -> Option<usize> {
        if range.len() == 0 {
            return None;
        }

        let mut tallies = vec![];
        for i in range {
            if i == 0 {
                // only 1 way to create an empty vec
                tallies.push(1.0);
            } else {
                // number of ways to create a vec of lenth i
                tallies.push(i as f64 * cardinality);
            }
        }

        self.pick_variant(&tallies)
    }

    /// Pick a variant from `0..weights.len()`, where variant `i` has cardinality `weights[i]`.
    /// We will tend to pick the "heavier" variants more often, thus spending more time in the
    /// variants that have more values to explore.
    fn pick_variant(&mut self, weights: &[f64]) -> Option<usize> {
        let value = self.driver.fuzz_f64(0.0..weights.iter().sum())?;
        let mut start = 0.0;
        for (weight, i) in weights.iter().zip(0..) {
            let end = start + *weight;
            if (start..end).contains(&value) {
                return Some(i);
            }
            start = end;
        }
        None
    }
}

/// Guard value given when you invoke one of the `enter_` routines for a given type.
/// Gives access to some of the metadata around that type.
pub struct EnterGuard<'a, 'd, L: Language, T: 'static> {
    cx: &'a mut FuzzCx<'d, L>,
    depth: Depth,
    phantom: PhantomData<T>,
}

impl<'d, L: Language, T: 'static> Deref for EnterGuard<'_, 'd, L, T> {
    type Target = FuzzCx<'d, L>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

impl<'d, L: Language, T: 'static> EnterGuard<'_, 'd, L, T> {
    pub fn depth(&self) -> Depth {
        self.depth
    }

    /// Number of available values set in the fuzz context for this type.
    /// Used for identifier types and other things that pick from a known universe.
    pub fn num_available_values(&mut self) -> usize {
        let id = TypeId::of::<T>();
        let Some(values) = self.cx.valid_values.get(&id) else {
            return 0;
        };
        let values = values.downcast_ref::<Vec<T>>().unwrap();
        values.len()
    }

    /// Pick one of the available values set in the fuzz context for this type.
    /// Used for identifier types and other things that pick from a known universe.
    #[track_caller]
    pub fn pick_value(&mut self) -> Option<T>
    where
        T: Clone,
    {
        let id = TypeId::of::<T>();
        let Some(values) = self.cx.config.valid_values.get(&id) else {
            panic!("no values set for `{}`)", std::any::type_name::<T>(),)
        };
        let values = values.downcast_ref::<Vec<T>>().unwrap();
        if values.is_empty() {
            return None;
        }
        let i = self.cx.driver.fuzz_usize(0..values.len())?;
        Some(values[i].clone())
    }

    /// Pick a free variable from among those introduced by binders in scope (if any).
    pub fn pick_free_variable(&mut self) -> Option<CoreVariable<L>> {
        let i: usize = self.cx.driver.fuzz_usize(0..self.free_variables.len())?;
        Some(self.free_variables[i].clone())
    }

    /// Introduce a set of free variables onto the stack available for use when fuzzing.
    /// The number of variables introduces is controlled by the `binder_range` configured on the [`FuzzCx`][].
    /// Invoke the closure `op` to produce a result and return that result, after popping variables from the internal list.
    pub fn introduce_free_variables_of_kinds<R>(
        &mut self,
        kinds: &[L::Kind],
        op: impl FnOnce(&mut Self, &[CoreVariable<L>]) -> Option<R>,
    ) -> Option<R> {
        // Fuzz them
        let free_variables: Vec<CoreVariable<L>> = kinds
            .iter()
            .map(|&kind| fresh_bound_var(kind).upcast())
            .collect();

        // Introduce the free variables, remembering the old length
        let old_len = self.cx.free_variables.len();
        self.cx.free_variables.extend(&free_variables);

        let result = op(self, &free_variables);

        self.cx.free_variables.truncate(old_len);

        result
    }

    /// Returns an estimate of the number of free variables that will be available for use.
    pub fn estimated_free_variable_cardinality(&mut self) -> f64 {
        // Each binder may introduce up to N items.
        let per_binder = self.cx.binder_range.len() as f64;

        self.depth.binders as f64 * per_binder
    }

    /// Generate a fuzzed value of type `F`.
    pub fn estimate_cardinality<F: Fuzzable<L>>(&mut self) -> f64 {
        F::estimate_cardinality(self.cx)
    }

    /// Estimate the cardinality of generating `range` elements of `F`
    pub fn estimate_cardinality_many<F: Fuzzable<L>>(
        &mut self,
        range: std::ops::Range<usize>,
    ) -> f64 {
        let element = self.estimate_cardinality::<F>();
        range.map(|i| element.powi(i as i32)).sum()
    }

    /// Generate a fuzzed value of type `F`.
    pub fn fuzz<F: Fuzzable<L>>(&mut self) -> Option<F> {
        F::fuzz(self.cx)
    }

    /// Generate a fuzzed value of type `F`.
    pub fn fuzz_many<F: Fuzzable<L>, C: FromIterator<F>>(
        &mut self,
        range: std::ops::Range<usize>,
    ) -> Option<C> {
        let cardinality = self.estimate_cardinality::<F>();
        let len = self.cx.pick_count(range, cardinality)?;
        (0..len).map(|_| self.fuzz::<F>()).collect()
    }

    pub fn fuzz_sum_type(
        &mut self,
        weights: &[f64],
        constructors: &[&dyn Fn(&mut EnterGuard<'_, '_, L, T>) -> Option<T>],
    ) -> Option<T> {
        assert_eq!(weights.len(), constructors.len());
        let v = self.cx.pick_variant(weights)?;
        constructors[v](self)
    }

    pub fn fuzz_u32(&mut self, range: std::ops::Range<u32>) -> Option<u32> {
        self.cx.driver.fuzz_u32(range)
    }

    pub fn fuzz_usize(&mut self, range: std::ops::Range<usize>) -> Option<usize> {
        self.cx.driver.fuzz_usize(range)
    }

    pub fn fuzz_f64(&mut self, range: std::ops::Range<f64>) -> Option<f64> {
        self.cx.driver.fuzz_f64(range)
    }
}

pub trait Fuzzable<L: Language>: Sized + 'static {
    /// Return true if this fuzzable will introduce binders.
    /// This will help us reach more accurate estimates.
    fn is_binder() -> bool {
        false
    }

    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64;

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self>;
}

impl<L: Language, T> Fuzzable<L> for Vec<T>
where
    T: Fuzzable<L>,
{
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
            guard.estimate_cardinality_many::<T>(guard.collection_range())
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| guard.fuzz_many(guard.collection_range.clone()))
    }
}

impl<L: Language, T> Fuzzable<L> for Set<T>
where
    T: Fuzzable<L> + Ord,
{
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
            guard.cx.collection_range.len() as f64 * guard.estimate_cardinality::<T>()
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        let len = cx.driver.fuzz_usize(cx.collection_range.clone())?;
        (0..len).map(|_| T::fuzz(cx)).collect()
    }
}

impl<L: Language, K, V> Fuzzable<L> for Map<K, V>
where
    K: Fuzzable<L> + Ord,
    V: Fuzzable<L>,
{
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
                (guard.estimate_cardinality::<K>()
                * guard.estimate_cardinality::<V>())
                .powi(guard.cx.collection_range.len() as i32)
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        let len = cx.driver.fuzz_usize(cx.collection_range.clone())?;
        (0..len)
            .map(|_| Some((K::fuzz(cx)?, V::fuzz(cx)?)))
            .collect()
    }
}

impl<L: Language> Fuzzable<L> for () {
    fn estimate_cardinality(_cx: &mut FuzzCx<'_, L>) -> f64 {
        1.0
    }

    fn fuzz(_cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        Some(())
    }
}

macro_rules! tuple_impl {
    ($($A:ident),*) => {
        impl<L: Language, $($A,)*> Fuzzable<L> for ($($A,)*)
        where
            $($A: Fuzzable<L>,)*
        {
            fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
                cx.enter_estimate_cardinality::<Self>(|guard| -> f64 {
                    [
                        $(guard.estimate_cardinality::<$A>(),)*
                    ].into_iter().product()
                })
            }

            fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
                cx.enter_fuzz::<Self>(|guard| {
                    Some(($(guard.fuzz::<$A>()?,)*))
                })
            }

        }
    }
}

tuple_impl!(A);
tuple_impl!(A, B);
tuple_impl!(A, B, C);
tuple_impl!(A, B, C, D);
tuple_impl!(A, B, C, D, E);
tuple_impl!(A, B, C, D, E, F);
tuple_impl!(A, B, C, D, E, F, G);
tuple_impl!(A, B, C, D, E, F, G, H);
tuple_impl!(A, B, C, D, E, F, G, H, I);
tuple_impl!(A, B, C, D, E, F, G, H, I, J);
tuple_impl!(A, B, C, D, E, F, G, H, I, J, K);
tuple_impl!(A, B, C, D, E, F, G, H, I, J, K, L0);
tuple_impl!(A, B, C, D, E, F, G, H, I, J, K, L0, M);

impl<L: Language, T> Fuzzable<L> for Arc<T>
where
    T: Fuzzable<L>,
{
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        T::estimate_cardinality(cx)
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        Some(Arc::new(T::fuzz(cx)?))
    }
}

impl<L: Language, T> Fuzzable<L> for Box<T>
where
    T: Fuzzable<L>,
{
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        T::estimate_cardinality(cx)
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        Some(Box::new(T::fuzz(cx)?))
    }
}

impl<L: Language, T> Fuzzable<L> for Option<T>
where
    T: Fuzzable<L>,
{
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| guard.estimate_cardinality_many::<T>(0..1))
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let mut v: Vec<T> = guard.fuzz_many(0..1)?;
            Some(v.pop())
        })
    }
}

impl<L: Language> Fuzzable<L> for usize {
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| guard.cx.literal_range().len() as f64)
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let range = guard.cx.literal_range();
            let i = guard
                .cx
                .driver
                .fuzz_usize(range.start as usize..range.end as usize)?;
            Some(i)
        })
    }
}

impl<L: Language> Fuzzable<L> for u32 {
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| guard.cx.literal_range().len() as f64)
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let range = guard.cx.literal_range();
            let i = guard.cx.driver.fuzz_u32(range)?;
            Some(i)
        })
    }
}

impl<L: Language> Fuzzable<L> for u128 {
    fn estimate_cardinality(cx: &mut FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| guard.cx.literal_range().len() as f64)
    }

    fn fuzz(cx: &mut FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let range = guard.cx.literal_range();
            let i = guard.cx.driver.fuzz_u32(range)?;
            Some(i as u128)
        })
    }
}

impl<'d, D> FuzzDriver<'d> for &'d mut D
where
    D: bolero::Driver,
{
    fn fuzz_u32(&mut self, range: std::ops::Range<u32>) -> Option<u32> {
        range.generate(&mut **self)
    }

    fn fuzz_usize(&mut self, range: std::ops::Range<usize>) -> Option<usize> {
        range.generate(&mut **self)
    }

    fn fuzz_f64(&mut self, range: std::ops::Range<f64>) -> Option<f64> {
        range.generate(&mut **self)
    }
}
