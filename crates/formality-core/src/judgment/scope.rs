use super::ProvenSet;

/// A scope that conditions of an inference rule can be proven in, with the
/// `(scope(<expr> => <pat>) with(<vars>) <conditions>)` condition of
/// [`judgment_fn!`](crate::judgment_fn).
///
/// `<expr>` enters the scope: it evaluates to the scope and to what the
/// nested conditions see inside it, as a pair. A scope typically extends an
/// environment for them, e.g. with the variables of a binder.
///
/// `Values` is the type of what they prove: the tuple of the `with`
/// variables.
pub trait Scope<Values> {
    /// `values`, proven inside the scope, as they hold outside of it. There
    /// may be several ways for them to, or none.
    fn leave(&self, values: Values) -> ProvenSet<Values>;
}
