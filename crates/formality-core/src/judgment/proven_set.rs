use crate::{judgment::IfThen, set, Map, Set};
use std::{
    fmt::Debug,
    hash::{Hash, Hasher},
    panic::Location,
};

/// Represents a set of items that were successfully proven using a judgment.
/// If the set is empty, then tracks the reason that the judgment failed for diagnostic purposes.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[must_use]
pub struct ProvenSet<J> {
    data: ProvenSetData<J>,
}

#[derive(Clone)]
enum ProvenSetData<J> {
    Failure(Box<FailedJudgment>),
    Success(Map<J, ProofTree>),
}

impl<J> From<ProvenSetData<J>> for ProvenSet<J> {
    fn from(data: ProvenSetData<J>) -> Self {
        ProvenSet { data }
    }
}

impl<J> From<FailedJudgment> for ProvenSet<J> {
    fn from(failure: FailedJudgment) -> Self {
        ProvenSetData::<J>::Failure(Box::new(failure)).into()
    }
}

impl<J: Ord + Debug + Clone> ProvenSet<J> {
    /// Creates a judgment set with a single item that was successfully proven.
    pub fn singleton(item: Proven<J>) -> Self {
        Self::proven(std::iter::once(item).collect())
    }

    /// Creates a judgment set with a set of `T` items that were successfully proven.
    /// The set should be non-empty.
    pub fn proven(data: Map<J, ProofTree>) -> Self {
        assert!(!data.is_empty());
        ProvenSetData::Success(data).into()
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
            ProvenSetData::Failure(_) => false,
            ProvenSetData::Success(s) => {
                assert!(!s.is_empty());
                true
            }
        }
    }

    /// Convert to a non-empty set of proven results (if ok) or an error (otherwise).
    pub fn into_map(self) -> Result<Map<J, ProofTree>, Box<FailedJudgment>> {
        match self.data {
            ProvenSetData::Failure(e) => Err(e),
            ProvenSetData::Success(s) => {
                assert!(!s.is_empty());
                Ok(s)
            }
        }
    }

    /// Extract the single proven result from this set.
    /// Panics if the set contains more than one result.
    /// Returns an error if the judgment failed.
    pub fn into_singleton(self) -> Result<Proven<J>, Box<FailedJudgment>> {
        match self.data {
            ProvenSetData::Failure(e) => Err(e),
            ProvenSetData::Success(mut s) => {
                assert!(s.len() == 1, "expected singleton, got {} results", s.len());
                Ok(s.pop_first().unwrap())
            }
        }
    }

    /// Iterate through all solutions.
    pub fn iter(&self) -> Box<dyn Iterator<Item = Proven<J>> + '_> {
        match &self.data {
            ProvenSetData::Failure(_) => Box::new(std::iter::empty()),
            ProvenSetData::Success(s) => Box::new(
                s.iter()
                    .map(|(judgment, tree)| (J::clone(judgment), tree.clone())),
            ),
        }
    }

    /// For each item `t` that was proven,
    /// invoke `op(t)` to yield a new set of proven results
    /// and then flatten those into a new proven set.
    /// This function preserves failure cause information and is the preferred way to chain
    /// sets.
    #[track_caller]
    pub fn flat_map<Iterable, K>(self, mut op: impl FnMut(Proven<J>) -> Iterable) -> ProvenSet<K>
    where
        Iterable: EachProof<Judgment = K>,
        K: Ord + Debug + Clone,
    {
        match self.data {
            ProvenSetData::Failure(e) => ProvenSet {
                data: ProvenSetData::Failure(e),
            },
            ProvenSetData::Success(proven_items) => {
                let mut items = Map::default();
                let mut failures = set![];

                for proven_item in proven_items {
                    let collection = op(proven_item);
                    if let Err(cause) = collection.each_proof(|(item, proof_tree)| {
                        items.insert(item, proof_tree);
                    }) {
                        failures.insert(FailedRule::new(cause));
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
    pub fn map<K>(self, mut op: impl FnMut(Proven<J>) -> Proven<K>) -> ProvenSet<K>
    where
        K: Ord + Debug + Clone,
    {
        self.flat_map::<_, K>(|elem| ProvenSet::singleton(op(elem)))
    }

    /// Convenience function for tests: asserts that the proven values match expected.
    #[track_caller]
    pub fn assert_ok(&self, expect: expect_test::Expect) {
        self.assert_ok_with(expect, &[])
    }

    /// Convenience function for tests: asserts that the proven values match expected,
    /// and that the proof tree contains all the required strings.
    #[track_caller]
    pub fn assert_ok_with(&self, expect_values: expect_test::Expect, must_contain: &[&str]) {
        match &self.data {
            ProvenSetData::Failure(e) => panic!("expected a successful proof, got {e}"),
            ProvenSetData::Success(map) => {
                // Check values only (not proof trees)
                let values: Set<_> = map.keys().cloned().collect();
                expect_values.assert_eq(&format!("{values:?}"));

                // Check proof tree contains required strings
                let full_output = format!("{map:?}");
                for s in must_contain {
                    assert!(
                        full_output.contains(s),
                        "proof tree must contain {s:?} but didn't.\nFull output:\n{full_output}"
                    );
                }
            }
        }
    }

    /// Convenience function for tests: asserts that the proven set is ok and that the debug value is as expected.
    /// Shows only the leaf failures for a concise view.
    #[track_caller]
    pub fn assert_err(&self, expect: expect_test::Expect) {
        match &self.data {
            ProvenSetData::Failure(e) => {
                expect.assert_eq(&crate::test_util::normalize_paths(e.format_leaves()));
            }
            ProvenSetData::Success(_) => {
                panic!("expected an error, got successful proofs: {self}");
            }
        }
    }
}

impl ProvenSet<()> {
    /// For cases where the "value" is just `()`, we can just extract the singular proof-tree directly
    pub fn check_proven(self) -> Result<ProofTree, Box<FailedJudgment>> {
        match self.data {
            ProvenSetData::Failure(e) => Err(e),
            ProvenSetData::Success(mut map) => Ok(map.remove(&()).expect("non-empty")),
        }
    }
}

impl<J: Ord + Debug + Clone> FromIterator<Proven<J>> for ProvenSet<J> {
    fn from_iter<T: IntoIterator<Item = Proven<J>>>(iter: T) -> Self {
        let set: Map<J, ProofTree> = iter.into_iter().collect();
        if set.is_empty() {
            ProvenSet::failed("collect", "empty collection")
        } else {
            ProvenSet::proven(set)
        }
    }
}

impl<J: PartialEq> PartialEq for ProvenSetData<J> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Failure(l0), Self::Failure(r0)) => format!("{l0:?}") == format!("{r0:?}"),
            (Self::Success(l0), Self::Success(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<J: std::fmt::Debug> std::fmt::Debug for ProvenSet<J> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { data } = self;
        std::fmt::Debug::fmt(data, f)
    }
}

impl<J: std::fmt::Debug> std::fmt::Debug for ProvenSetData<J> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Failure(arg0) => std::fmt::Debug::fmt(arg0, f),
            Self::Success(arg0) => std::fmt::Debug::fmt(arg0, f),
        }
    }
}

impl<J: Eq> Eq for ProvenSetData<J> {}

impl<J: PartialOrd> PartialOrd for ProvenSetData<J> {
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

impl<J: Ord> Ord for ProvenSetData<J> {
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

impl<J: Hash> Hash for ProvenSetData<J> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ProvenSetData::Failure(e) => e.to_string().hash(state),
            ProvenSetData::Success(s) => s.hash(state),
        }
    }
}

pub type Proven<J> = (J, ProofTree);

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
#[must_use]
pub struct ProofTree {
    /// Name of the judgment being proved
    pub judgment_name: String,

    /// Attributes as field: value pairs
    pub attributes: Vec<(String, String)>,

    /// Succeeded with this rule-name and index, if Some;
    /// if None, then there is only a single rule and this is not relevant.
    pub rule_name: Option<&'static str>,

    /// ...located in this file...
    pub file: String,

    /// ...at this line...
    pub line: u32,

    /// ...and this column...
    pub column: u32,

    /// ...with these subproofs.
    pub children: Vec<ProofTree>,
}

impl ProofTree {
    /// Create a leaf "proof tree" from a value.
    ///
    /// The origin will be the caller of this function
    /// and there won't be any child trees.
    #[track_caller]
    pub fn leaf(judgment: impl ToString) -> Self {
        Self::new(judgment, None, Vec::new())
    }

    pub fn with_all(
        judgment_name: impl ToString,
        attributes: Vec<(String, String)>,
        rule_name: Option<&'static str>,
        file: impl ToString,
        line: u32,
        column: u32,
        children: Vec<ProofTree>,
    ) -> Self {
        Self {
            judgment_name: judgment_name.to_string(),
            attributes,
            rule_name,
            file: file.to_string(),
            line,
            column,
            children,
        }
    }

    /// Create a "proof tree" from a value.
    /// The origin will be the caller of this function.
    #[track_caller]
    pub fn new(
        judgment: impl ToString,
        rule_name: Option<&'static str>,
        children: Vec<ProofTree>,
    ) -> Self {
        let caller = Location::caller();
        let proof = ProofTree {
            judgment_name: judgment.to_string(),
            attributes: Vec::new(),
            rule_name,
            file: caller.file().to_string(),
            line: caller.line(),
            column: caller.column(),
            children,
        };
        proof
    }

    /// Create a "proof tree" with explicit attributes.
    /// The origin will be the caller of this function.
    #[track_caller]
    pub fn new_with_attributes(
        judgment_name: impl ToString,
        attributes: Vec<(String, String)>,
        rule_name: Option<&'static str>,
        children: Vec<ProofTree>,
    ) -> Self {
        let caller = Location::caller();
        ProofTree {
            judgment_name: judgment_name.to_string(),
            attributes,
            rule_name,
            file: caller.file().to_string(),
            line: caller.line(),
            column: caller.column(),
            children,
        }
    }

    /// Total number of nodes in the proof tree.
    pub fn total_nodes(&self) -> usize {
        1 + self.children.iter().map(|c| c.total_nodes()).sum::<usize>()
    }
}

/// Insert a proof tree into the map, keeping the smaller tree if one already exists.
/// Comparison is by total nodes first, then by string representation as tiebreaker.
pub fn insert_smallest_proof<K: Ord + Clone>(
    map: &mut Map<K, ProofTree>,
    key: K,
    proof: ProofTree,
) {
    match map.get(&key) {
        Some(existing) => {
            let dominated = (proof.total_nodes(), format!("{:?}", proof))
                < (existing.total_nodes(), format!("{:?}", existing));
            if dominated {
                map.insert(key, proof);
            }
        }
        None => {
            map.insert(key, proof);
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

    /// Extract "leaf" failures - the actual terminal failure causes
    /// rather than the full nested tree of failed judgments.
    pub fn leaf_failures(&self) -> Vec<&FailedRule> {
        let mut leaves = Vec::new();
        for rule in &self.failed_rules {
            rule.collect_leaves(&mut leaves);
        }
        leaves
    }

    /// Format just the leaf failures in a concise way
    pub fn format_leaves(&self) -> String {
        let leaves = self.leaf_failures();
        if leaves.is_empty() {
            format!("judgment had no applicable rules: `{}`", self.judgment)
        } else {
            leaves
                .iter()
                .map(|leaf| leaf.to_string())
                .collect::<Vec<_>>()
                .join("\n\n")
        }
    }
}

impl FailedRule {
    /// Recursively collect leaf failures (failures whose cause is not another FailedJudgment)
    fn collect_leaves<'a>(&'a self, leaves: &mut Vec<&'a FailedRule>) {
        match &self.cause {
            RuleFailureCause::FailedJudgment(inner) => {
                for rule in &inner.failed_rules {
                    rule.collect_leaves(leaves);
                }
            }
            _ => {
                // This is a leaf - the cause is a terminal condition
                leaves.push(self);
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct FailedRule {
    /// If Some, then the given rule failed
    /// (if None, then there is only a single rule and this is not relevant)...
    pub rule_name: Option<String>,

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
        let location = Location::caller();
        FailedRule {
            rule_name: None,
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
    IfFalse(IfThen),

    /// The rule did not succeed because an `(if let)` pattern failed to match.
    IfLetDidNotMatch { pattern: String, value: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was an empty collection.
    EmptyCollection { expr: String },

    /// The rule did not succeed because the `x` in a `(x => y)` rule was a judgment that failed
    /// (for the given reason).
    FailedJudgment(Box<FailedJudgment>),

    /// The rule did not succeed for some custom reason. This occurs when a `?` fails in the judgment function macro.
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
            rule_name,
            file,
            line,
            column,
            cause,
        } = self;

        if let Some(rule_name) = rule_name {
            write!(
                f,
                "the rule {rule_name:?} at ({file}:{line}:{column}) failed because\n{cause}",
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
            RuleFailureCause::IfFalse(IfThen { cond, args }) => {
                write!(f, "condition evaluted to false: `{cond}`")?;
                for (arg_expr, arg_value) in args {
                    write!(f, "\n  {arg_expr} = {arg_value:?}")?;
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
            ProvenSetData::Failure(err) => std::fmt::Display::fmt(err, f),
            ProvenSetData::Success(set) => {
                writeln!(f, "{{")?;
                for (judgment, proof_tree) in set {
                    writeln!(f, "  {judgment:?}")?;
                    proof_tree.fmt_indented(f, "    ")?;
                }
                writeln!(f, "}}")?;
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for ProofTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_indented(f, "")
    }
}

impl std::fmt::Debug for ProofTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl ProofTree {
    fn fmt_indented(&self, f: &mut std::fmt::Formatter<'_>, prefix: &str) -> std::fmt::Result {
        let file_name = self.file.rsplit('/').next().unwrap_or(&self.file);
        let rule_info = match self.rule_name {
            Some(name) => format!(" ({name})"),
            None => String::new(),
        };

        // Print judgment name with rule info and location
        writeln!(
            f,
            "{prefix}└─ {}:{rule_info} at {file_name}:{}",
            self.judgment_name, self.line
        )?;

        // Print attributes indented under the judgment name
        let attr_prefix = format!("{prefix}       ");
        for (field, value) in &self.attributes {
            writeln!(f, "{attr_prefix}{field}: {value}")?;
        }

        let child_prefix = format!("{prefix}   ");
        for child in &self.children {
            child.fmt_indented(f, &child_prefix)?;
        }
        Ok(())
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
pub trait EachProof {
    type Judgment;

    /// If the iterable is non-empty, invokes `each_proof` for each item and returns `Ok(())`.
    /// Otherwise, returns `Err(_)` with a description of the failure;
    /// `stringify_expr` is used to create that description, it should
    /// return a string representing the expression being enumerated.
    #[track_caller]
    fn each_proof(
        self,
        each_proof: impl FnMut(Proven<Self::Judgment>),
    ) -> Result<(), RuleFailureCause>;
}

impl<T> EachProof for ProvenSet<T> {
    type Judgment = T;

    fn each_proof(
        self,
        mut each_proof: impl FnMut(Proven<Self::Judgment>),
    ) -> Result<(), RuleFailureCause> {
        match self.data {
            ProvenSetData::Failure(e) => Err(RuleFailureCause::FailedJudgment(e)),
            ProvenSetData::Success(s) => {
                for item in s {
                    each_proof(item);
                }
                Ok(())
            }
        }
    }
}

impl<T: Clone> EachProof for &ProvenSet<T> {
    type Judgment = T;

    fn each_proof(
        self,
        mut each_proof: impl FnMut(Proven<Self::Judgment>),
    ) -> Result<(), RuleFailureCause> {
        match &self.data {
            ProvenSetData::Failure(e) => Err(RuleFailureCause::FailedJudgment(e.clone())),
            ProvenSetData::Success(s) => {
                for (key, proof) in s {
                    each_proof((key.clone(), proof.clone()));
                }
                Ok(())
            }
        }
    }
}

/// Proof rule for `(x in collection)`.
/// Invokes `each_proof` for each member of `collection`.
///
/// # Parameters
///
/// * `collection` is the value to be iterated.
/// * `stringify_expr` gives a string for error message purposes, in case the collection is empty
/// * `each_proof` is the action to take on each item
pub fn member_of<T: Debug>(
    collection: impl IntoIterator<Item = T>,
    stringify_expr: impl FnOnce() -> String,
    mut each_proof: impl FnMut(Proven<T>),
) -> Result<(), RuleFailureCause> {
    let mut iter = collection.into_iter().peekable();
    if iter.peek().is_none() {
        Err(RuleFailureCause::EmptyCollection {
            expr: stringify_expr(),
        })
    } else {
        for item in iter {
            let proof_tree = ProofTree::leaf(format!("item = {item:?}"));
            each_proof((item, proof_tree));
        }
        Ok(())
    }
}

pub trait CheckProven {
    /// If the iterable is non-empty, invokes `each_proof` for each item and returns `Ok(())`.
    /// Otherwise, returns `Err(_)` with a description of the failure;
    /// `stringify_expr` is used to create that description, it should
    /// return a string representing the expression being enumerated.
    #[track_caller]
    fn check_proven(self) -> Result<ProofTree, RuleFailureCause>;
}

impl CheckProven for ProvenSet<()> {
    #[track_caller]
    fn check_proven(self) -> Result<ProofTree, RuleFailureCause> {
        self.check_proven()
            .map_err(|e| RuleFailureCause::FailedJudgment(e.clone()))
    }
}

impl CheckProven for &ProvenSet<()> {
    #[track_caller]
    fn check_proven(self) -> Result<ProofTree, RuleFailureCause> {
        self.clone()
            .check_proven()
            .map_err(|e| RuleFailureCause::FailedJudgment(e.clone()))
    }
}
