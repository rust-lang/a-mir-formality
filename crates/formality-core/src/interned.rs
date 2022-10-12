use std::hash::Hash;
use std::sync::Mutex;
use std::{collections::HashMap, sync::Arc};

#[derive(Debug)]
pub struct Interned<T> {
    data: Arc<T>,
}

pub struct Interner<T> {
    table: Mutex<HashMap<T, Interned<T>>>,
}

impl<T> Default for Interner<T> {
    fn default() -> Self {
        Self {
            table: Default::default(),
        }
    }
}

impl<T> Interner<T> {
    pub fn intern(&self, data: &T) -> Interned<T>
    where
        T: Clone + Eq + Hash,
    {
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

impl<T> std::ops::Deref for Interned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &*self.data
    }
}

impl<T> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.data, &other.data)
    }
}

impl<T> Eq for Interned<T> {}

impl<T> PartialOrd for Interned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let this = Arc::as_ptr(&self.data) as usize;
        let other = Arc::as_ptr(&other.data) as usize;
        PartialOrd::partial_cmp(&this, &other)
    }
}

impl<T> Ord for Interned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let this = Arc::as_ptr(&self.data) as usize;
        let other = Arc::as_ptr(&other.data) as usize;
        Ord::cmp(&this, &other)
    }
}

impl<T> Hash for Interned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(Arc::as_ptr(&self.data) as usize);
    }
}

impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Interned {
            data: self.data.clone(),
        }
    }
}
