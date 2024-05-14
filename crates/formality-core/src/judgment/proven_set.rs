use crate::{set, Set};
use std::{
    fmt::Debug,
    hash::{Hash, Hasher},
};

/// Represents a set of items that were successfully proven using a judgment.
/// If the set is empty, then tracks the reason that the judgment failed for diagnostic purposes.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[must_use]
pub struct ProvenSet<T> {
    data: Data<T>,
}

#[derive(Clone)]
enum Data<T> {
    Failure(Box<FailedJudgment>),
    Success(Set<T>),
}

impl<T> From<Data<T>> for ProvenSet<T> {
    fn from(data: Data<T>) -> Self {
        ProvenSet { data }
    }
}

impl<T> From<FailedJudgment> for ProvenSet<T> {
    fn from(failure: FailedJudgment) -> Self {
        Data::Failure(Box::new(failure)).into()
    }
}

impl<T: Ord + Debug> ProvenSet<T> {
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
        FailedJudgment::new(
            description.to_string(),
            set![FailedRule::new(RuleFailureCause::Inapplicable {
                reason: reason.to_string()
            })],
        )
        .into()
    }

    /// Creates a judgment set that resulted from a failed judgment.
    /// Meant to be used from the judgment macro, probably annoying to call manually.
    pub fn failed_rules(judgment: impl std::fmt::Debug, failed_rules: Set<FailedRule>) -> Self {
        let judgment = format!("{judgment:?}");
        FailedJudgment::new(judgment, failed_rules).into()
    }

    /// True if the judgment whose result this set represents was proven at least once.
    pub fn is_proven(&self) -> bool {
        match &self.data {
            Data::Failure(_) => false,
            Data::Success(s) => {
                assert!(!s.is_empty());
                true
            }
        }
    }

    pub fn check_proven(self) -> Result<(), Box<FailedJudgment>> {
        match self.data {
            Data::Failure(e) => Err(e),
            Data::Success(_) => Ok(()),
        }
    }

    /// Convert to a non-empty set of proven results (if ok) or an error (otherwise).
    pub fn into_set(self) -> Result<Set<T>, Box<FailedJudgment>> {
        match self.data {
            Data::Failure(e) => Err(e),
            Data::Success(s) => {
                assert!(!s.is_empty());
                Ok(s)
            }
        }
    }

    /// Iterate through all solutions.
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
        U: Ord + Debug,
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
                    ProvenSet::failed_rules("flat_map", failures)
                }
            }
        }
    }

    /// For each item `t` that was proven,
    /// invoke `op(t)` to yield a new item `u`
    /// and create a proven set from that.
    /// This function preserves failure cause information.
    #[track_caller]
    pub fn map<U>(self, mut op: impl FnMut(T) -> U) -> ProvenSet<U>
    where
        U: Ord + Debug,
    {
        self.flat_map(|elem| set![op(elem)])
    }

    /// Convenience function for tests: asserts that the proven set is ok and that the debug value is as expected.
    #[track_caller]
    pub fn assert_ok(&self, expect: expect_test::Expect) {
        match &self.data {
            Data::Failure(e) => panic!("expected a successful proof, got {e}"),
            Data::Success(_) => {
                expect.assert_eq(&self.to_string());
            }
        }
    }

    /// Convenience function for tests: asserts that the proven set is ok and that the debug value is as expected.
    #[track_caller]
    pub fn assert_err(&self, expect: expect_test::Expect) {
        match &self.data {
            Data::Failure(e) => {
                expect.assert_eq(&crate::test_util::normalize_paths(e));
            }
            Data::Success(_) => {
                panic!("expected an error, got successful proofs: {self}");
            }
        }
    }
}

impl<I: Ord + Debug> FromIterator<I> for ProvenSet<I> {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let set: Set<I> = iter.into_iter().collect();
        if set.is_empty() {
            ProvenSet::failed("collect", "empty collection")
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct FailedJudgment {
    /// Trying to prove this judgment...
    pub judgment: String,

    /// ...failed with these partially applicable rules.
    /// If empty, it means no rules matched.
    pub failed_rules: Set<FailedRule>,
}

#[derive(Debug)]
struct HasNonCycle(bool);

impl FailedJudgment {
    /// Create a new "failed judgment" representing a failure to
    /// solve `judgment` with the given set of partially solved rules.
    ///
    /// Does a bit of post-processing to detect cycles,
    /// since some of these rule failures may represent a cyclic attempt
    /// to solve judgment. We employ the heuristic that we only present
    /// cycle errors if ALL the rule failes are cycles. Otherwise, we
    /// show only the non-cyclic failures, as usually that's what the user
    /// is interested in.
    #[tracing::instrument(level = "Debug", ret)]
    fn new(judgment: String, failed_rules: Set<FailedRule>) -> Self {
        // Strip cycles out from the set of failed rules. Note that this algorithm
        // has an O(n^2) character, as the set of failed rules will get recursively
        // simplified as we progress up the stack, but .. who cares?
        let (failed_rules, _) = Self::strip_cycles(set![&judgment], failed_rules);
        Self {
            judgment,
            failed_rules,
        }
    }

    /// Simplifies a set of failed rules to exclude cycles, but only if there are non-cyclic results to show instead.
    /// Recursively Given a set of failed rules, along with the `stack` of judgments that was being solved at the time these failures occurred,
    /// returns `(set, has_non_cycle)`. The `set` represents a new, simplified set of failued
    #[tracing::instrument(level = "Debug", skip(stack), ret)]
    fn strip_cycles(
        stack: Set<&String>,
        failed_rules: Set<FailedRule>,
    ) -> (Set<FailedRule>, HasNonCycle) {
        // Collect all the failures that were due to cycles
        let mut cycles = set![];

        // Collect failures not due to cycles
        let mut non_cycles = set![];

        // Go over each failure and insert it into the appropriate entry above
        for mut failed_rule in failed_rules {
            let span = tracing::debug_span!("failed_rule", ?failed_rule);
            let _guard = span.enter();

            if let RuleFailureCause::FailedJudgment(mut judgment) = failed_rule.cause {
                // Recursive case: we failed because a judgment failed...
                if stack.contains(&judgment.judgment) && judgment.failed_rules.is_empty() {
                    // ...if that judgment was already on the stack, and we didn't have a more interesting reason,
                    // then this is a failure.
                    failed_rule.cause = RuleFailureCause::Cycle {
                        judgment: judgment.judgment.clone(),
                    };
                    cycles.insert(failed_rule);
                } else {
                    // ...otherwise, recursively simplify the failed rules.
                    // This will return a boolean indicating if all the failed rules
                    // ultimately failed because of a cycle.

                    let mut stack1 = stack.clone();
                    stack1.insert(&judgment.judgment);

                    let judgment_has_non_cycle;
                    (judgment.failed_rules, judgment_has_non_cycle) =
                        Self::strip_cycles(stack1, judgment.failed_rules);
                    failed_rule.cause = RuleFailureCause::FailedJudgment(judgment);

                    if judgment_has_non_cycle.0 {
                        non_cycles.insert(failed_rule);
                    } else {
                        // If all the failed rules failed because of a cycle,
                        // then this judgment is itself a cycle.
                        cycles.insert(failed_rule);
                    }
                }
            } else if let RuleFailureCause::Cycle { .. } = failed_rule.cause {
                cycles.insert(failed_rule);
            } else {
                non_cycles.insert(failed_rule);
            }
        }

        tracing::debug!(?cycles, ?non_cycles);

        if non_cycles.is_empty() {
            (cycles, HasNonCycle(false))
        } else {
            (non_cycles, HasNonCycle(true))
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
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
    IfFalse {
        /// The stringified form of the expression.
        expr: String,

        /// A set of pairs with the stringified form of arguments within the expression plus the debug representation of its value.
        /// This is a best effort extraction via the macro.
        args: Vec<(String, String)>,
    },

    /// The rule did not succeed because an `(if let)` pattern failed to match.
    IfLetDidNotMatch { pattern: String, value: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was an empty collection.
    EmptyCollection { expr: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was a judgment that failed
    /// (for the given reason).
    FailedJudgment(Box<FailedJudgment>),

    /// The rule did not succeed for some custom reason; this is not generated by the macro.
    Inapplicable { reason: String },

    /// The rule attempted to prove something that was already in the process of being proven
    Cycle { judgment: String },
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
            let rules: Vec<String> = failed_rules.iter().map(|r| r.to_string()).collect();
            let rules = indent(rules.join("\n"));
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
            RuleFailureCause::IfFalse { expr, args } => {
                write!(f, "condition evaluted to false: `{expr}`")?;
                for (arg_expr, arg_value) in args {
                    write!(f, "\n  {arg_expr} = {arg_value}")?;
                }
                Ok(())
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
            RuleFailureCause::Cycle { judgment } => {
                write!(f, "cyclic proof attempt: `{judgment}`")
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
    let lines: Vec<String> = s
        .lines()
        .map(|l| format!("  {l}"))
        .map(|l| l.trim_end().to_string())
        .collect();
    lines.join("\n")
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
