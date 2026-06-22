use super::env::Env;
use crate::grammar::Variable;
use crate::rust::Visit;
use formality_core::{Upcast, UpcastFrom};

/// A wrapper around a value that is constrained by a set of conditions.
///
/// We sometimes use `Constrained<()>` as a "upcast-able synonym" for `Constraints`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Constrained<T>(pub T, pub Env);

impl<T> Constrained<T> {
    pub fn none(env: Env, value: T) -> Self {
        Self(value, env.into())
    }

    pub fn constraints(&self) -> &Env {
        &self.1
    }

    pub fn unconditionally_true(&self) -> bool {
        self.constraints().unconditionally_true()
    }
}

impl<T: Clone, U> UpcastFrom<Constrained<T>> for Constrained<U>
where
    T: Upcast<U>,
{
    fn upcast_from(Constrained(t, c): Constrained<T>) -> Self {
        Constrained(t.upcast(), c)
    }
}

pub fn occurs_in(v: impl Upcast<Variable>, t: &impl Visit) -> bool {
    let v: Variable = v.upcast();
    t.free_variables().contains(&v)
}

impl UpcastFrom<Env> for Constrained<()> {
    fn upcast_from(e: Env) -> Self {
        Constrained((), e)
    }
}
