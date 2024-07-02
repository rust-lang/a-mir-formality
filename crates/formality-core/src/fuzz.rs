//! See the Formality Book [chapter on fuzzing][f] for more details.
//!
//! [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html

use std::fmt::Debug;
use std::sync::OnceLock;
use std::{ops::Deref, sync::RwLock};

use crate::Map;

/// A global singleton accessible from anywhere.
/// Used to thread state for fuzzing.
///
/// See the Formality Book [chapter on fuzzing][f] for more details.
///
/// [f]: https://rust-lang.github.io/a-mir-formality/formality_core/fuzzing.html
pub struct FuzzSingleton<I> {
    data: OnceLock<RwLock<I>>,
}

/// Trait for clearing the value of a singleton.
/// Not invoked from outside.
trait FuzzClear {
    fn clear(&self);
}

/// Guard that clears the singleton back to its default value after `set` is called.
pub struct SetGuard<'s> {
    pool: &'s dyn FuzzClear,
}

impl<I> FuzzSingleton<I>
where
    I: Debug + Default + Eq,
{
    /// Create a new instance; this is a `const` so that the value can be stored in a `static`.
    pub const fn new() -> Self {
        Self {
            data: OnceLock::new(),
        }
    }

    /// Internal method: access the data, initializing if needed.
    fn data(&self) -> &RwLock<I> {
        self.data.get_or_init(|| RwLock::new(I::default()))
    }

    /// Set the set of available names.
    ///
    /// This is intended to be called once per fuzzing session,
    /// so it will panic if the value has already been "set" from its default value.
    ///
    /// Returns a guard that will restore the value to the default when dropped.
    /// Once this guard is dropped, you can call `set` again.
    pub fn set(&self, new_data: I) -> SetGuard<'_> {
        let mut data = self.data().write().unwrap();
        let old_data = std::mem::replace(&mut *data, new_data);
        assert!(
            I::default() == old_data,
            "cannot fuzz more than one program at a time, already fuzzing `{old_data:?}`"
        );
        SetGuard { pool: self }
    }

    /// Access the data. Will deadlock if you try to set before the guard is dropped.
    pub fn get(&self) -> impl Deref<Target = I> + '_ {
        self.data().read().unwrap()
    }
}

impl<I> FuzzClear for FuzzSingleton<I>
where
    I: Debug + Default + Eq,
{
    /// Clear the set of available names.
    /// Invoked by the guard returned from `set`.
    fn clear(&self) {
        *self.data().write().unwrap() = Default::default();
    }
}

impl<K: Debug + Ord, V: Debug + Eq + Clone> FuzzSingleton<Map<K, V>>
where
    K: Debug + Ord,
    V: Debug + Clone,
{
    pub fn get_key(&self, key: K) -> V {
        self.get().get(&key).cloned().unwrap()
    }
}

impl<E: Clone + Debug + Eq> FuzzSingleton<Vec<E>> {
    /// Pick one of the available names from the fuzzer,
    /// returning `None` if there are no available names or the fuzzer
    /// ran out of data.
    pub fn fuzz_pick(&self, driver: &mut impl bolero::Driver) -> Option<E> {
        let data = self.get();
        if data.is_empty() {
            return None;
        }
        let i = driver.gen_variant(data.len(), 0)?;
        Some(data[i].clone())
    }

    /// Push names into the pool of available values that will be used later
    /// by `fuzz_pick`. Returns a guard that will pop them from the pool.
    pub fn fuzz_push(&self, values: Vec<E>) -> PushGuard<'_, E> {
        let mut data = self.data().write().unwrap();
        let len = data.len();
        data.extend(values);
        PushGuard { pool: self, len }
    }
}

/// Guard to pop names added by `fuzz_push`.
pub struct PushGuard<'s, E: Clone + Debug + Eq> {
    pool: &'s FuzzSingleton<Vec<E>>,
    len: usize,
}

impl<E> Drop for PushGuard<'_, E>
where
    E: Clone + Debug + Eq,
{
    fn drop(&mut self) {
        let mut data = self.pool.data().write().unwrap();
        data.truncate(self.len);
    }
}

impl Drop for SetGuard<'_> {
    fn drop(&mut self) {
        self.pool.clear();
    }
}
