/// Wrapper that provides `Ord`/`Eq`/`Hash` for types that implement `Debug`
/// but not `Ord`. Comparison is done via debug string representation.
///
/// Used to wrap `libspecr` / MiniRust types so they can appear in
/// `judgment_fn!` signatures (which require `Ord + Hash + Clone + Debug`).
#[derive(Clone)]
pub struct OrdByDebug<T: std::fmt::Debug + Clone>(pub T);

impl<T: std::fmt::Debug + Clone> OrdByDebug<T> {
    fn debug_str(&self) -> String {
        format!("{:?}", self.0)
    }
}

impl<T: std::fmt::Debug + Clone> std::fmt::Debug for OrdByDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: std::fmt::Debug + Clone> PartialEq for OrdByDebug<T> {
    fn eq(&self, other: &Self) -> bool {
        self.debug_str() == other.debug_str()
    }
}

impl<T: std::fmt::Debug + Clone> Eq for OrdByDebug<T> {}

impl<T: std::fmt::Debug + Clone> PartialOrd for OrdByDebug<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: std::fmt::Debug + Clone> Ord for OrdByDebug<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.debug_str().cmp(&other.debug_str())
    }
}

impl<T: std::fmt::Debug + Clone> std::hash::Hash for OrdByDebug<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.debug_str().hash(state)
    }
}

impl<T: std::fmt::Debug + Clone> formality_core::Upcast<OrdByDebug<T>> for OrdByDebug<T> {
    fn upcast(self) -> OrdByDebug<T> {
        self
    }
}
