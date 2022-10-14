use std::hash::Hash;
use std::sync::Mutex;
use std::{collections::HashMap, sync::Arc};

#[derive(Debug)]
pub struct Interned<T: Internable> {
    data: Arc<T>,
}

impl<T: Internable> Interned<T> {
    pub fn new(data: T) -> Self {
        T::table().intern(&data)
    }
}

impl<T: Internable> From<T> for Interned<T> {
    fn from(data: T) -> Self {
        T::table().intern(&data)
    }
}

pub trait Internable: Clone + Eq + Hash + 'static {
    fn table() -> &'static Interner<Self>;
}

pub struct Interner<T: Internable> {
    table: Mutex<HashMap<T, Interned<T>>>,
}

impl<T: Internable> Default for Interner<T> {
    fn default() -> Self {
        Self {
            table: Default::default(),
        }
    }
}

impl<T: Internable> Interner<T> {
    pub fn intern(&self, data: &T) -> Interned<T> {
        let mut table = self.table.lock().unwrap();
        if let Some(i) = table.get(data) {
            return i.clone();
        }

        let interned = Interned {
            data: Arc::new(data.clone()),
        };
        table.insert(data.clone(), interned.clone());
        interned
    }
}

impl<T: Internable> std::ops::Deref for Interned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &*self.data
    }
}

impl<T: Internable> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.data, &other.data)
    }
}

impl<T: Internable> Eq for Interned<T> {}

impl<T: Internable> PartialOrd for Interned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let this = Arc::as_ptr(&self.data) as usize;
        let other = Arc::as_ptr(&other.data) as usize;
        PartialOrd::partial_cmp(&this, &other)
    }
}

impl<T: Internable> Ord for Interned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let this = Arc::as_ptr(&self.data) as usize;
        let other = Arc::as_ptr(&other.data) as usize;
        Ord::cmp(&this, &other)
    }
}

impl<T: Internable> Hash for Interned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(Arc::as_ptr(&self.data) as usize);
    }
}

impl<T: Internable> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Interned {
            data: self.data.clone(),
        }
    }
}

impl Internable for String {
    fn table() -> &'static Interner<Self> {
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<String> = Interner::default();
        }
        &*INTERNER
    }
}
