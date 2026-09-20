//! Root-scoped memoization for completed judgment evaluations.
//!
//! The judgment runtime computes recursive judgments by repeatedly executing
//! their rules until the set of proven output values reaches a fixed point.
//! During that process it distinguishes two kinds of reuse, checked in this
//! order by [`execute_judgment`](super::execute_judgment):
//!
//! 1. An *active recursive call* returns the current approximation from the
//!    runtime's execution stack. That cycle-breaking cache lives in
//!    [`runtime`](super::runtime), not in this module.
//! 2. An *iteration memo hit* reuses a nested judgment that already completed
//!    while all enclosing fixed-point approximations were unchanged.
//!
//! ## Iteration lifetime
//!
//! Every fresh judgment evaluation calls [`IterationGuard::begin`], which
//! pushes an empty [`ErasedTables`] slot onto [`MemoContext::active_iterations`].
//! Nested judgments accumulate their completed positive results in that slot.
//! If the judgment discovers new output values and must execute its rules
//! again, [`IterationGuard::invalidate`] replaces the slot with an empty table:
//! descendants completed under the old approximation are no longer reusable.
//!
//! Once the judgment reaches a fixed point, [`IterationGuard::complete`] pops
//! its slot. For a nested judgment, the valid descendant entries and the
//! judgment's own positive result are merged into the parent's slot. For a
//! top-level judgment, the slot is discarded. Memoized results therefore live
//! only for the duration of one top-level judgment call and cannot affect a
//! later call or test.
//!
//! [`IterationGuard`] also makes the stack unwind-safe. It remembers the stack
//! depth that it owns and truncates back to that depth if rule execution
//! panics before normal completion.
//!
//! ## What is cached
//!
//! Only positive results are cached. An empty [`ProofMap`] means that no value
//! was proven, but it does not contain the failure diagnostics needed to treat
//! that result as a reusable negative fact.
//!
//! A `judgment_fn!` expansion creates a distinct input type for each judgment.
//! [`ErasedTables`] uses that input type's [`TypeId`] as the judgment identity,
//! allowing one context to hold heterogeneous [`MemoTable`] values without
//! allowing judgments with otherwise identical signatures to collide. The
//! context is thread-local, matching the thread-local judgment execution
//! stacks and avoiding synchronization between independent evaluations.

use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
};

use crate::Map;

use super::{insert_smallest_proof, ProofTree};

/// The proven values and their smallest known proof trees for one judgment input.
type ProofMap<Output> = Map<Output, ProofTree>;

/// Common bounds required for values stored in a type-erased memo table.
///
/// This private trait keeps the erased-table signatures readable; its blanket
/// implementation does not add behavior beyond the listed bounds.
trait MemoValue: Clone + Eq + Debug + Hash + 'static {}
impl<T: Clone + Eq + Debug + Hash + 'static> MemoValue for T {}

/// Whether iteration lookups are enabled.
///
/// Production execution always uses [`MemoMode::Enabled`]. Tests temporarily
/// select [`MemoMode::Disabled`] to compute an uncached reference result.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum MemoMode {
    /// Bypass iteration memo lookups.
    Disabled,

    /// Reuse valid entries completed during this top-level call.
    #[default]
    Enabled,
}

/// All memoization state for the current thread.
///
/// Keeping the iteration stack and test mode in one `RefCell` gives each lookup
/// or lifecycle transition one scoped mutable borrow.
#[derive(Default)]
struct MemoContext {
    /// One provisional table set for every fresh judgment evaluation currently active.
    active_iterations: Vec<ErasedTables>,

    /// Test-controlled switch for comparing memoized and uncached execution.
    mode: MemoMode,
}

thread_local! {
    // Judgment evaluation itself is thread-local, so memo entries cannot be
    // observed by another thread and require no synchronization.
    static MEMO_CONTEXT: RefCell<MemoContext> = RefCell::new(MemoContext::default());
}

/// Mutably access the current thread's memo context for one short operation.
///
/// Callers must not recursively enter this helper from `op`, because the
/// `RefCell` remains mutably borrowed until `op` returns.
fn with_context<R>(op: impl FnOnce(&mut MemoContext) -> R) -> R {
    MEMO_CONTEXT.with(|context| op(&mut context.borrow_mut()))
}

/// Look up a positive result completed in the current chain of valid iterations.
///
/// The search proceeds from the innermost active iteration to the outermost.
/// This favors the entry established in the most local evaluation while still
/// allowing a nested call to reuse work already completed by an ancestor. At a
/// given depth, entries remain valid until that depth is invalidated; completing
/// the guard may transfer them into its still-valid parent.
///
/// Returns `None` when memoization is disabled, when no matching entry exists,
/// or when the matching judgment previously failed (failures are not cached).
pub(crate) fn lookup_iteration<Input, Output>(input: &Input) -> Option<ProofMap<Output>>
where
    Input: Clone + Eq + Debug + Hash + 'static,
    Output: Clone + Eq + Debug + Hash + Ord + 'static,
{
    with_context(|context| {
        if context.mode == MemoMode::Disabled {
            return None;
        }

        context
            .active_iterations
            .iter()
            .rev()
            .find_map(|entries| entries.get::<Input, Output>(input))
            .cloned()
    })
}

/// Return the number of provisional iteration-table sets currently on the stack.
///
/// Panic-safety tests use this to verify that [`IterationGuard::drop`] removed
/// every table owned by an unwound judgment evaluation.
#[cfg(test)]
pub(crate) fn active_iteration_count() -> usize {
    with_context(|context| context.active_iterations.len())
}

/// Test-only scope that selects memoized or uncached execution for this thread.
///
/// The mode may change only between top-level judgment calls, when there are no
/// memo tables to preserve or discard.
#[cfg(test)]
pub(crate) struct TestModeGuard {
    previous: MemoMode,
}

#[cfg(test)]
impl TestModeGuard {
    /// Select whether completed results may be reused during the next root call.
    pub(crate) fn new(enabled: bool) -> Self {
        let previous = with_context(|context| {
            assert!(
                context.active_iterations.is_empty(),
                "cannot change memo mode during judgment evaluation",
            );
            std::mem::replace(
                &mut context.mode,
                if enabled {
                    MemoMode::Enabled
                } else {
                    MemoMode::Disabled
                },
            )
        });
        Self { previous }
    }
}

#[cfg(test)]
impl Drop for TestModeGuard {
    /// Restore the mode that was active before this test scope.
    fn drop(&mut self) {
        with_context(|context| {
            assert!(
                context.active_iterations.is_empty(),
                "memo mode scope ended during judgment evaluation",
            );
            context.mode = self.previous;
        });
    }
}

/// Owns the provisional memo-table slot for one fresh judgment evaluation.
///
/// `depth` is both the stack length observed by [`IterationGuard::begin`] and
/// the index of this guard's [`ErasedTables`] slot after it is pushed. It lets
/// every lifecycle operation assert that guards complete in stack order and
/// lets unwinding truncate all state created at or below this evaluation.
/// `active` distinguishes a normally completed guard, whose slot was already
/// popped, from one that must clean up during `Drop`.
pub(crate) struct IterationGuard {
    /// Stack length before this guard pushed its slot, and therefore that slot's index.
    depth: usize,

    /// Whether `Drop` still owns cleanup of this guard's slot and descendants.
    active: bool,
}

impl IterationGuard {
    /// Begin a fresh judgment evaluation with an empty provisional table set.
    ///
    /// The returned guard must be completed after the fixed-point loop succeeds.
    /// If it is dropped first, it treats the evaluation as unwound and removes
    /// everything pushed since this call.
    pub(crate) fn begin() -> Self {
        let depth = with_context(|context| {
            let depth = context.active_iterations.len();
            context.active_iterations.push(ErasedTables::default());
            depth
        });
        Self {
            depth,
            active: true,
        }
    }

    /// Assert that this guard owns the topmost, still-empty iteration table.
    ///
    /// The runtime checks this at the start of every rule pass. [`Self::begin`]
    /// establishes the invariant for the first pass, and [`Self::invalidate`]
    /// restores it before each repeated pass. The table may then accumulate only
    /// completed descendant executions while that pass evaluates its rules.
    pub(crate) fn assert_is_top_and_empty(&self) {
        with_context(|context| {
            assert_eq!(
                context.active_iterations.len(),
                self.depth + 1,
                "current judgment does not own the top memo iteration",
            );
            assert!(
                context.active_iterations[self.depth].is_empty(),
                "current judgment's memo iteration is not empty before a rule pass",
            );
        });
    }

    /// Discard entries completed under an obsolete fixed-point approximation.
    ///
    /// The runtime calls this whenever the judgment gains proven output values
    /// and has recursive dependents, just before executing its rules again. The
    /// stack slot itself remains in place so subsequent descendants can repopulate
    /// it under the new approximation.
    pub(crate) fn invalidate(&self) {
        with_context(|context| {
            assert_eq!(context.active_iterations.len(), self.depth + 1);
            context.active_iterations[self.depth] = ErasedTables::default();
        });
    }

    /// Finish this judgment and transfer all results computed in its valid iteration.
    ///
    /// The guard's own table contains completed descendants but not the judgment
    /// represented by `input`; that result is supplied separately as `output`.
    /// If a parent iteration exists, both are merged into its provisional table
    /// for possible reuse by later nested calls. Otherwise this was a top-level
    /// evaluation, so `completed_descendants` is dropped and no memoized result
    /// survives the call. Empty outputs are ignored by [`ErasedTables::insert`].
    pub(crate) fn complete<Input, Output>(mut self, input: &Input, output: &ProofMap<Output>)
    where
        Input: Clone + Eq + Debug + Hash + 'static,
        Output: Clone + Eq + Debug + Hash + Ord + 'static,
    {
        let completed_descendants = with_context(|context| {
            assert_eq!(context.active_iterations.len(), self.depth + 1);
            context.active_iterations.pop().unwrap()
        });
        self.active = false;

        with_context(|context| {
            if let Some(parent) = context.active_iterations.last_mut() {
                parent.merge(completed_descendants);
                parent.insert(input.clone(), output.clone());
            }
        });
    }
}

impl Drop for IterationGuard {
    /// Remove provisional tables when evaluation exits without calling `complete`.
    ///
    /// Truncation, rather than a single pop, also removes any descendant state
    /// that remains if a panic interrupts nested judgment execution.
    fn drop(&mut self) {
        if self.active {
            with_context(|context| {
                assert!(context.active_iterations.len() > self.depth);
                context.active_iterations.truncate(self.depth);
            });
        }
    }
}

/// A heterogeneous collection containing at most one memo table per judgment.
///
/// Each `judgment_fn!` expansion creates a unique input type, so its [`TypeId`]
/// identifies the judgment even when another judgment has the same Rust input
/// and output shapes. Values are boxed behind [`ErasedMemoTable`] so a single
/// iteration can contain every judgment encountered by the current root call.
#[derive(Default)]
struct ErasedTables {
    /// Concrete [`MemoTable`] values indexed by their generated judgment input type.
    by_judgment: HashMap<TypeId, Box<dyn ErasedMemoTable>>,
}

impl ErasedTables {
    /// Find the completed positive result for one judgment input.
    ///
    /// The input type selects the concrete table and the input value selects an
    /// entry within it. A table's output type is fixed by its judgment; a failed
    /// downcast therefore indicates an internal identity/type mismatch rather
    /// than a normal cache miss.
    fn get<Input, Output>(&self, input: &Input) -> Option<&ProofMap<Output>>
    where
        Input: MemoValue,
        Output: MemoValue + Ord,
    {
        self.by_judgment
            .get(&TypeId::of::<Input>())?
            .as_any()
            .downcast_ref::<MemoTable<Input, Output>>()
            .expect("judgment identity mapped to an unexpected memo-table type")
            .entries
            .get(input)
    }

    /// Insert or merge a completed positive judgment result.
    ///
    /// Empty proof maps are deliberately excluded because their failure diagnostics are not
    /// represented in the map. Treating them as cached negative facts would be unsound.
    /// Returns `true` when a positive entry was inserted or merged and `false`
    /// when `output` was empty and therefore ignored.
    fn insert<Input, Output>(&mut self, input: Input, output: ProofMap<Output>) -> bool
    where
        Input: MemoValue,
        Output: MemoValue + Ord,
    {
        if output.is_empty() {
            return false;
        }

        let identity = TypeId::of::<Input>();
        let table = self
            .by_judgment
            .entry(identity)
            .or_insert_with(|| Box::new(MemoTable::<Input, Output>::default()));
        table
            .as_any_mut()
            .downcast_mut::<MemoTable<Input, Output>>()
            .expect("judgment identity mapped to an unexpected memo-table type")
            .insert(input, output);
        true
    }

    /// Transfer every judgment table and entry from `other` into this collection.
    ///
    /// This is used to bubble a completed descendant set into its parent
    /// iteration.
    /// Colliding entries are delegated to [`ErasedMemoTable::merge`], which
    /// verifies that their proven values agree and combines proof metadata.
    fn merge(&mut self, other: ErasedTables) {
        for (identity, other_table) in other.by_judgment {
            match self.by_judgment.get_mut(&identity) {
                Some(table) => table.merge(other_table),
                None => {
                    self.by_judgment.insert(identity, other_table);
                }
            }
        }
    }

    /// Report whether this collection contains no concrete tables.
    ///
    /// Because empty outputs are never inserted, this also means it contains no
    /// cached positive entries.
    fn is_empty(&self) -> bool {
        self.by_judgment.is_empty()
    }
}

/// Object-safe operations needed to store and merge differently typed memo tables.
///
/// The `Any` accessors recover a concrete [`MemoTable`] after its generated
/// input [`TypeId`] selects the appropriate trait object. `merge` is exposed
/// through the trait because [`ErasedTables`] must perform that operation
/// without knowing the table's type parameters.
trait ErasedMemoTable: Any {
    /// Borrow this table as `Any` for a checked shared downcast.
    fn as_any(&self) -> &dyn Any;

    /// Borrow this table as `Any` for a checked mutable downcast.
    fn as_any_mut(&mut self) -> &mut dyn Any;

    /// Convert an owned erased table into `Any` for an owned downcast.
    fn into_any(self: Box<Self>) -> Box<dyn Any>;

    /// Merge another table selected by the same judgment identity into this one.
    fn merge(&mut self, other: Box<dyn ErasedMemoTable>);
}

/// Completed positive results for one concrete judgment.
///
/// `Input` is the unique generated judgment input type as well as the per-entry
/// key. `Output` is the judgment's proven-value type. Each input maps to all
/// proven outputs and the smallest proof tree retained for each output.
struct MemoTable<Input, Output> {
    /// Positive results indexed by the complete judgment input value.
    entries: HashMap<Input, ProofMap<Output>>,
}

impl<Input, Output> Default for MemoTable<Input, Output> {
    /// Create an empty concrete judgment table.
    fn default() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
}

impl<Input, Output> MemoTable<Input, Output>
where
    Input: MemoValue,
    Output: MemoValue + Ord,
{
    /// Insert one result, merging proof metadata if the input already exists.
    ///
    /// Two valid completions of the same judgment input must prove exactly the
    /// same output values. [`merge_equivalent_outputs`] asserts that invariant
    /// and retains the smaller proof tree for each value.
    fn insert(&mut self, input: Input, output: ProofMap<Output>) {
        match self.entries.entry(input) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                merge_equivalent_outputs(entry.get_mut(), output);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(output);
            }
        }
    }
}

impl<Input, Output> ErasedMemoTable for MemoTable<Input, Output>
where
    Input: MemoValue,
    Output: MemoValue + Ord,
{
    /// Expose this concrete table for shared type recovery by [`ErasedTables::get`].
    fn as_any(&self) -> &dyn Any {
        self
    }

    /// Expose this concrete table for mutable type recovery by [`ErasedTables::insert`].
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    /// Expose this owned table so `merge` can recover its concrete entry map.
    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }

    /// Merge all entries from another table with the same generated identity.
    ///
    /// The enclosing [`ErasedTables`] map matched these objects by input
    /// [`TypeId`], so a downcast failure indicates an internal invariant breach.
    fn merge(&mut self, other: Box<dyn ErasedMemoTable>) {
        let other = other
            .into_any()
            .downcast::<Self>()
            .expect("cannot merge memo tables with different concrete types");
        for (input, output) in other.entries {
            self.insert(input, output);
        }
    }
}

/// Merge two valid completions of the same judgment input.
///
/// Memoization must not change the semantic set of proven values, so colliding
/// entries are required to have identical ordered keys. Proof trees are metadata
/// and may differ; for each value, this retains the smallest proof observed.
fn merge_equivalent_outputs<Output: Ord + Clone>(
    current: &mut ProofMap<Output>,
    next: ProofMap<Output>,
) {
    assert!(
        current.keys().eq(next.keys()),
        "equivalent memo entries have different proven values",
    );

    for (value, proof) in next {
        insert_smallest_proof(current, value, proof);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    struct InputA(u32);

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    struct InputB(u32);

    fn small_proof() -> ProofTree {
        ProofTree::leaf("small")
    }

    fn large_proof() -> ProofTree {
        ProofTree::new("large", None, vec![ProofTree::leaf("child")])
    }

    #[test]
    fn typed_table_round_trip_and_empty_exclusion() {
        let mut tables = ErasedTables::default();

        assert!(!tables.insert::<InputA, u32>(InputA(0), Map::new()));
        assert!(tables.is_empty());

        let output = Map::from([(22_u32, small_proof())]);
        assert!(tables.insert(InputA(0), output.clone()));
        assert_eq!(tables.get::<InputA, u32>(&InputA(0)), Some(&output));
    }

    #[test]
    fn generated_input_type_isolates_judgment_identity() {
        let mut tables = ErasedTables::default();

        tables.insert(InputA(0), Map::from([(22_u32, small_proof())]));
        tables.insert(InputB(0), Map::from([(44_u32, small_proof())]));

        assert!(tables
            .get::<InputA, u32>(&InputA(0))
            .is_some_and(|output| output.contains_key(&22)));
        assert!(tables
            .get::<InputB, u32>(&InputB(0))
            .is_some_and(|output| output.contains_key(&44)));
    }

    #[test]
    fn table_merge_retains_the_smallest_proof() {
        let mut left = ErasedTables::default();
        left.insert(InputA(0), Map::from([(22_u32, large_proof())]));

        let mut right = ErasedTables::default();
        let small = small_proof();
        right.insert(InputA(0), Map::from([(22_u32, small.clone())]));

        left.merge(right);

        assert_eq!(
            left.get::<InputA, u32>(&InputA(0))
                .and_then(|output| output.get(&22)),
            Some(&small),
        );
    }

    #[test]
    #[should_panic(expected = "equivalent memo entries have different proven values")]
    fn table_merge_rejects_semantically_unequal_collision() {
        let mut left = ErasedTables::default();
        left.insert(InputA(0), Map::from([(22_u32, small_proof())]));

        let mut right = ErasedTables::default();
        right.insert(InputA(0), Map::from([(44_u32, small_proof())]));

        left.merge(right);
    }

    #[test]
    fn completing_a_root_discards_all_memoized_entries() {
        let _mode = TestModeGuard::new(true);
        let root = IterationGuard::begin();
        let child = IterationGuard::begin();
        let child_output = Map::from([(22_u32, small_proof())]);

        child.complete(&InputA(0), &child_output);
        assert_eq!(lookup_iteration(&InputA(0)), Some(child_output));

        root.complete(&InputB(0), &Map::from([(44_u32, small_proof())]));
        assert_eq!(active_iteration_count(), 0);
        assert_eq!(lookup_iteration::<InputA, u32>(&InputA(0)), None);
    }
}
