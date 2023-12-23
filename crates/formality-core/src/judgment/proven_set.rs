use crate::{set, Set};
use std::{
    fmt::Debug,
    hash::{Hash, Hasher},
    sync::Arc,
};

/// Represents a set of items that were successfully proven using a judgment.
/// If the set is empty, then tracks the reason that the judgment failed for diagnostic purposes.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProvenSet<T> {
    data: Data<T>,
}

#[derive(Clone)]
enum Data<T> {
    Failure(Arc<FailedJudgment>),
    Success(Set<T>),
}

impl<T> From<Data<T>> for ProvenSet<T> {
    fn from(data: Data<T>) -> Self {
        ProvenSet { data }
    }
}

impl<T: Ord> ProvenSet<T> {
    /// Creates a judgment set with a single item that was successfully proven.
    pub fn singleton(item: T) -> Self {
        Self::proven(set![item])
    }

    /// Creates a judgment set with a set of `T` items that were successfully proven.
    /// The set should be non-empty.
    pub fn proven(data: Set<T>) -> Self {
        assert!(!data.is_empty());
        Data::Success(data).into()
    }

    /// Creates a `JudgmentSet` from a Rust function that failed for the given reason.
    #[track_caller]
    pub fn failed(description: impl std::fmt::Display, reason: impl std::fmt::Display) -> Self {
        Data::Failure(Arc::new(FailedJudgment {
            judgment: description.to_string(),
            failed_rules: set![FailedRule::new(RuleFailureCause::Inapplicable {
                reason: reason.to_string()
            })],
        }))
        .into()
    }

    /// Creates a judgment set that resulted from a failed judgment.
    /// Meant to be used from the judgment macro, probably annoying to call manually.
    pub fn failed_rules(judgment: &impl std::fmt::Debug, failed_rules: Set<FailedRule>) -> Self {
        Data::Failure(Arc::new(FailedJudgment {
            judgment: format!("{judgment:?}"),
            failed_rules,
        }))
        .into()
    }

    /// True if the judgment whose result this set represents was proven at least once.
    pub fn is_proven(&self) -> bool {
        !self.is_empty()
    }

    /// True if nothing was proven (i.e., set is empty).
    pub fn is_empty(&self) -> bool {
        match &self.data {
            Data::Failure(_) => true,
            Data::Success(s) => s.is_empty(),
        }
    }

    /// Convert to a set of proven results, losing the failure cause information.
    pub fn into_set(self) -> Set<T> {
        match self.data {
            Data::Failure(_) => set![],
            Data::Success(s) => s,
        }
    }

    /// Iterate over proven results, losing the failure cause information.
    pub fn into_iter(self) -> <Set<T> as IntoIterator>::IntoIter {
        self.into_set().into_iter()
    }

    /// Iterate over proven results, losing the failure cause information.
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a T> + 'a> {
        match &self.data {
            Data::Failure(_) => Box::new(std::iter::empty()),
            Data::Success(s) => Box::new(s.iter()),
        }
    }

    /// For each item `t` that was proven,
    /// invoke `op(t)` to yield a new set of proven results
    /// and then flatten those into a new proven set.
    /// This function preserves failure cause information and is the preferred way to chain
    /// sets.
    #[track_caller]
    pub fn flat_map<I, U>(self, mut op: impl FnMut(T) -> I) -> ProvenSet<U>
    where
        I: TryIntoIter<Item = U>,
        U: Ord,
    {
        match self.data {
            Data::Failure(e) => ProvenSet {
                data: Data::Failure(e),
            },
            Data::Success(set) => {
                let mut items = set![];
                let mut failures = set![];

                for item in set {
                    let collection = op(item);
                    match collection.try_into_iter(|| "flat_map".to_string()) {
                        Ok(iterator) => items.extend(iterator),
                        Err(cause) => {
                            failures.insert(FailedRule::new(cause));
                        }
                    }
                }

                if !items.is_empty() {
                    ProvenSet::proven(items)
                } else {
                    ProvenSet::failed_rules(&"flat_map", failures)
                }
            }
        }
    }
}

impl<I: Ord> FromIterator<I> for ProvenSet<I> {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let set: Set<I> = iter.into_iter().collect();
        if set.is_empty() {
            ProvenSet::failed(&"collect", &"empty collection")
        } else {
            ProvenSet::proven(set)
        }
    }
}

impl<T: PartialEq> PartialEq for Data<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Failure(l0), Self::Failure(r0)) => format!("{l0:?}") == format!("{r0:?}"),
            (Self::Success(l0), Self::Success(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ProvenSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { data } = self;
        std::fmt::Debug::fmt(data, f)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Data<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Failure(arg0) => std::fmt::Debug::fmt(arg0, f),
            Self::Success(arg0) => std::fmt::Debug::fmt(arg0, f),
        }
    }
}

impl<T: Eq> Eq for Data<T> {}

impl<T: PartialOrd> PartialOrd for Data<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Failure(l0), Self::Failure(r0)) => {
                PartialOrd::partial_cmp(&format!("{l0:?}"), &format!("{r0:?}"))
            }
            (Self::Success(l0), Self::Success(r0)) => PartialOrd::partial_cmp(l0, r0),
            (Self::Failure(_), Self::Success(_)) => Some(std::cmp::Ordering::Less),
            (Self::Success(_), Self::Failure(_)) => Some(std::cmp::Ordering::Greater),
        }
    }
}

impl<T: Ord> Ord for Data<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Failure(l0), Self::Failure(r0)) => {
                Ord::cmp(&format!("{l0:?}"), &format!("{r0:?}"))
            }
            (Self::Success(l0), Self::Success(r0)) => Ord::cmp(l0, r0),
            (Self::Failure(_), Self::Success(_)) => std::cmp::Ordering::Less,
            (Self::Success(_), Self::Failure(_)) => std::cmp::Ordering::Greater,
        }
    }
}

impl<T: Hash> Hash for Data<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Data::Failure(e) => e.to_string().hash(state),
            Data::Success(s) => s.hash(state),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FailedJudgment {
    /// Trying to prove this judgment...
    pub judgment: String,

    /// ...failed with these partially applicable rules.
    /// If empty, it means no rules matched.
    pub failed_rules: Set<FailedRule>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FailedRule {
    /// If Some, then the given rule failed at the given index
    /// (if None, then there is only a single rule and this is not relevant)...
    pub rule_name_index: Option<(String, usize)>,

    /// ...and is located in this file...
    pub file: String,

    /// ...at this line...
    pub line: u32,

    /// ...and this column...
    pub column: u32,

    /// ...for this reason...
    pub cause: RuleFailureCause,
}

impl FailedRule {
    #[track_caller]
    pub fn new(cause: RuleFailureCause) -> Self {
        let location = std::panic::Location::caller();
        FailedRule {
            rule_name_index: None,
            file: location.file().to_string(),
            line: location.line(),
            column: location.column(),
            cause,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum RuleFailureCause {
    /// The rule did not succeed because an `(if X)` condition evaluated to false.
    IfFalse { expr: String },

    /// The rule did not succeed because an `(if let)` pattern failed to match.
    IfLetDidNotMatch { pattern: String, value: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was an empty collection.
    EmptyCollection { expr: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was a judgment that failed
    /// (for the given reason).
    FailedJudgment(Arc<FailedJudgment>),

    /// The rule did not succeed for some custom reason; this is not generated by the macro.
    Inapplicable { reason: String },
}

impl std::error::Error for FailedJudgment {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl std::fmt::Display for FailedJudgment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FailedJudgment {
            judgment,
            failed_rules,
        } = self;
        if failed_rules.is_empty() {
            write!(f, "judgment had no applicable rules: `{judgment}` ",)
        } else {
            let rules: String = failed_rules.iter().map(|r| format!("{r}\n")).collect();
            let rules = indent(rules);
            write!(
                f,
                "judgment `{judgment}` failed at the following rule(s):\n{rules}"
            )
        }
    }
}

impl std::fmt::Display for FailedRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FailedRule {
            rule_name_index,
            file,
            line,
            column,
            cause,
        } = self;

        if let Some((rule_name, step_index)) = rule_name_index {
            write!(
                f,
                "the rule {rule_name:?} failed at step #{step_index} ({file}:{line}:{column}) because\n{cause}",
                cause = indent(cause),
            )
        } else {
            write!(
                f,
                "failed at ({file}:{line}:{column}) because\n{cause}",
                cause = indent(cause),
            )
        }
    }
}

impl std::fmt::Display for RuleFailureCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuleFailureCause::IfFalse { expr } => {
                write!(f, "condition evaluted to false: `{expr}`")
            }
            RuleFailureCause::IfLetDidNotMatch { pattern, value } => {
                write!(f, "pattern `{pattern}` did not match value `{value}`")
            }
            RuleFailureCause::EmptyCollection { expr } => {
                write!(f, "expression evaluated to an empty collection: `{expr}`")
            }
            RuleFailureCause::FailedJudgment(judgment) => std::fmt::Display::fmt(judgment, f),
            RuleFailureCause::Inapplicable { reason } => {
                write!(f, "{reason}")
            }
        }
    }
}

impl<T: Debug> std::fmt::Display for ProvenSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            Data::Failure(err) => std::fmt::Display::fmt(err, f),
            Data::Success(set) => {
                write!(f, "{{\n")?;
                for item in set {
                    write!(f, "{},\n", indent(format!("{item:?}")))?;
                }
                write!(f, "}}\n")?;
                Ok(())
            }
        }
    }
}

fn indent(s: impl std::fmt::Display) -> String {
    let s = s.to_string();
    let mut s = s.replace("\n", "\n  ");
    s.insert_str(0, "  ");
    s
}

/// This trait is used for the `(foo => bar)` patterns.
///
/// If `foo` evaluates to a non-empty set of `bar` elements,
/// returns an `Ok` iterator.
///
/// Otherwise, returns returns an error.
pub trait TryIntoIter {
    type IntoIter: Iterator<Item = Self::Item>;
    type Item;

    /// `value` is the result of the expression `foo`.
    ///
    /// `stringify_expr` is a closure that will return a string describing the expression that produced value.
    fn try_into_iter(
        self,
        stringify_expr: impl FnOnce() -> String,
    ) -> Result<Self::IntoIter, RuleFailureCause>;
}

impl<T> TryIntoIter for ProvenSet<T> {
    type IntoIter = <Set<T> as IntoIterator>::IntoIter;
    type Item = T;

    fn try_into_iter(
        self,
        _stringify_expr: impl FnOnce() -> String,
    ) -> Result<Self::IntoIter, RuleFailureCause> {
        match self.data {
            Data::Failure(e) => Err(RuleFailureCause::FailedJudgment(e)),
            Data::Success(s) => Ok(s.into_iter()),
        }
    }
}

impl<'a, T> TryIntoIter for &'a ProvenSet<T> {
    type IntoIter = <&'a Set<T> as IntoIterator>::IntoIter;
    type Item = &'a T;

    fn try_into_iter(
        self,
        _stringify_expr: impl FnOnce() -> String,
    ) -> Result<Self::IntoIter, RuleFailureCause> {
        match &self.data {
            Data::Failure(e) => Err(RuleFailureCause::FailedJudgment(e.clone())),
            Data::Success(s) => Ok(s.into_iter()),
        }
    }
}

impl<T: IntoIterator> TryIntoIter for T {
    type IntoIter = std::iter::Peekable<<T as IntoIterator>::IntoIter>;
    type Item = <T as IntoIterator>::Item;

    fn try_into_iter(
        self,
        stringify_expr: impl FnOnce() -> String,
    ) -> Result<Self::IntoIter, RuleFailureCause> {
        let mut iter = self.into_iter().peekable();
        if iter.peek().is_none() {
            Err(RuleFailureCause::EmptyCollection {
                expr: stringify_expr(),
            })
        } else {
            Ok(iter)
        }
    }
}
