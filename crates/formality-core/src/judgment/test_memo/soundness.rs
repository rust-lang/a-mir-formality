//! End-to-end memoization soundness checks.
//!
//! The focused tests in the sibling modules assert particular cache events.
//! These tests instead use uncached execution as an oracle and check that
//! memoization preserves the result. In addition to cyclic reachability from
//! independent roots, they systematically test small systems of mutually
//! recursive Boolean judgments.
//!
//! For example, one system we want to test could have been written by hand as
//! these three judgment functions:
//!
//! ```text
//! judgment_fn! {
//!     fn a() => () {
//!         (
//!             (b() => ())
//!             ----------- ("from b")
//!             (a() => ())
//!         )
//!         (
//!             (c() => ())
//!             ----------- ("from c")
//!             (a() => ())
//!         )
//!     }
//! }
//!
//! judgment_fn! {
//!     fn b() => () {
//!         (
//!             (a() => ())
//!             (c() => ())
//!             ----------- ("from a and c")
//!             (b() => ())
//!         )
//!     }
//! }
//!
//! judgment_fn! {
//!     fn c() => () {
//!         (
//!             ----------- ("unconditional")
//!             (c() => ())
//!         )
//!     }
//! }
//! ```
//!
//! In other words, `a` is proven if `b || c`, `b` is proven if `a && c`, and
//! `c` is always proven. Starting with no proven judgments, the fixed-point
//! computation first proves `c`, then `a` from `c`, and finally `b` from `a`
//! and `c`.
//!
//! The test represents each handwritten function by a truth table. The input
//! to the table is the set of judgments currently proven: bit 0 means `a`, bit
//! 1 means `b`, and bit 2 means `c`. The eight possible inputs range from
//! `0b000` (nothing proven) through `0b111` (all three proven). The output at
//! each input says whether that function's rules have enough premises to prove
//! its judgment:
//!
//! | Proven judgments | Input and output-bit index | Can `a` be proven? |
//! | ---------------- | -------------------------- | ------------------ |
//! | none             | `000`                      | no                 |
//! | `a`              | `001`                      | no                 |
//! | `b`              | `010`                      | yes                |
//! | `a, b`           | `011`                      | yes                |
//! | `c`              | `100`                      | yes                |
//! | `a, c`           | `101`                      | yes                |
//! | `b, c`           | `110`                      | yes                |
//! | `a, b, c`        | `111`                      | yes                |
//!
//! ```text
//! function for a: b || c  = 0b1111_1100
//! function for b: a && c = 0b1010_0000
//! function for c: true   = 0b1111_1111
//! ```
//!
//! For example, `a`'s byte has output bit 2 set because input `0b010` means
//! that `b` is proven, which is enough to use `a`'s `"from b"` rule. Its
//! minimal satisfying sets are `{b}` and `{c}`. `MonotoneSystem::clauses`
//! recovers those sets from the byte, and `monotone_node` evaluates each set as
//! the premises of one rule. Thus the parameterized `monotone_node(system,
//! node)` is the data-driven equivalent of the three functions above.
//!
//! There are only 256 possible eight-bit truth tables. `monotone_functions`
//! filters those down to the 20 monotone functions, and `MonotoneSystem` assigns
//! one of those functions independently to each of `a`, `b`, and `c`. The final
//! test can therefore enumerate all `20^3 = 8,000` three-judgment systems and
//! compare memoized execution with the uncached oracle for every starting order.

use std::sync::Arc;

use crate::{cast_impl, judgment_fn};

use super::{memo, values};

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct FiniteGraph {
    edges: Vec<(u32, u32)>,
}

cast_impl!(FiniteGraph);

impl FiniteGraph {
    fn successors(&self, node: u32) -> Vec<u32> {
        self.edges
            .iter()
            .filter_map(|(from, to)| (*from == node).then_some(*to))
            .collect()
    }
}

judgment_fn! {
    fn memo_reachable(graph: Arc<FiniteGraph>, from: u32) => u32 {
        debug(graph, from)

        (
            (to in graph.successors(*from))
            --- ("edge")
            (memo_reachable(graph, from) => to)
        )

        (
            (memo_reachable(graph, from) => intermediate)
            (memo_reachable(graph, intermediate) => to)
            --- ("transitive")
            (memo_reachable(graph, from) => to)
        )
    }
}

const BOOLEAN_NODES: [u8; 3] = [0, 1, 2];
const BOOLEAN_ROOT_ORDERS: [[u8; 3]; 6] = [
    [0, 1, 2],
    [0, 2, 1],
    [1, 0, 2],
    [1, 2, 0],
    [2, 0, 1],
    [2, 1, 0],
];

/// Three mutually recursive Boolean judgments.
///
/// Each function is represented by an eight-bit truth table indexed by the set
/// of variables that are true. Only monotone truth tables are used.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct MonotoneSystem {
    functions: [u8; 3],
}

cast_impl!(MonotoneSystem);

impl MonotoneSystem {
    /// Return the minimal satisfying sets for `node`'s function.
    ///
    /// These sets are the clauses in the function's canonical monotone DNF.
    /// An empty list is `false`; a list containing an empty clause is `true`.
    ///
    /// For the system in the module-level example, nodes `0`, `1`, and `2`
    /// stand for `a`, `b`, and `c`, respectively. This method recovers the
    /// premises of their handwritten rules:
    ///
    /// ```text
    /// clauses(0) = [[1], [2]]  // a follows from b, or from c
    /// clauses(1) = [[0, 2]]    // b follows from a and c
    /// clauses(2) = [[]]        // c follows without any premises
    /// ```
    fn clauses(&self, node: u8) -> Vec<Vec<u8>> {
        let function = self.functions[usize::from(node)];

        (0_u8..8)
            .filter(|&variables| function_holds(function, variables))
            .filter(|&variables| {
                !(0_u8..8).any(|subset| {
                    subset != variables
                        && subset & !variables == 0
                        && function_holds(function, subset)
                })
            })
            .map(|variables| {
                BOOLEAN_NODES
                    .into_iter()
                    .filter(|node| variables & (1_u8 << u32::from(*node)) != 0)
                    .collect()
            })
            .collect()
    }
}

fn function_holds(function: u8, variables: u8) -> bool {
    function & (1_u8 << u32::from(variables)) != 0
}

fn is_monotone_function(function: u8) -> bool {
    (0_u8..8).all(|variables| {
        (0_u8..8).all(|more_variables| {
            variables & !more_variables != 0
                || !function_holds(function, variables)
                || function_holds(function, more_variables)
        })
    })
}

fn monotone_functions() -> Vec<u8> {
    (u8::MIN..=u8::MAX)
        .filter(|function| is_monotone_function(*function))
        .collect()
}

fn clauses_hold(clauses: &[Vec<u8>], variables: u8) -> bool {
    clauses.iter().any(|clause| {
        clause
            .iter()
            .all(|node| variables & (1_u8 << u32::from(*node)) != 0)
    })
}

// This single parameterized judgment executes the rules recovered by
// `MonotoneSystem::clauses`. For `a` (node 0) in the module-level example, it
// considers clauses `[1]` and `[2]`. Recursively proving every dependency in
// `[1]` executes the handwritten "a from b" rule; doing the same for `[2]`
// executes "a from c". For `b`, its one clause `[0, 2]` requires both `a` and
// `c`. For `c`, the empty clause requires nothing and therefore succeeds
// unconditionally.
judgment_fn! {
    fn monotone_node(system: MonotoneSystem, node: u8) => () {
        debug(system, node)

        (
            (clause in system.clauses(*node))
            (for_all(dependency in clause)
                (monotone_node(system, dependency) => ()))
            --- ("minimal satisfying clause")
            (monotone_node(system, node) => ())
        )
    }
}

#[test]
fn independent_roots_match_uncached_evaluation_in_every_order() {
    let graph = Arc::new(FiniteGraph {
        edges: vec![(0, 1), (0, 2), (1, 3), (2, 3), (3, 1)],
    });
    let roots = [0, 1, 2, 3];
    let expected: Vec<_> = roots
        .iter()
        .map(|root| {
            let _mode = memo::TestModeGuard::new(false);
            values(memo_reachable(&graph, *root))
        })
        .collect();

    for order in [roots, [3, 2, 1, 0], [1, 3, 0, 2]] {
        let _mode = memo::TestModeGuard::new(true);
        for root in order {
            assert_eq!(
                values(memo_reachable(&graph, root)),
                expected[root as usize],
                "root={root}, order={order:?}",
            );
        }
    }
}

#[test]
fn memoized_positive_graphs_match_uncached_evaluation() {
    let graphs = [
        FiniteGraph {
            edges: vec![(0, 0)],
        },
        FiniteGraph {
            edges: vec![(0, 1), (1, 0), (1, 2)],
        },
        FiniteGraph {
            edges: vec![(0, 1), (0, 2), (1, 3), (2, 3), (3, 1)],
        },
    ];

    for graph in graphs {
        let graph = Arc::new(graph);
        for root in 0..=3 {
            let without_memo = {
                let _mode = memo::TestModeGuard::new(false);
                values(memo_reachable(&graph, root))
            };
            let with_memo = {
                let _mode = memo::TestModeGuard::new(true);
                values(memo_reachable(&graph, root))
            };
            assert_eq!(with_memo, without_memo, "graph={graph:?}, root={root}");
        }
    }
}

#[test]
fn all_three_node_monotone_systems_match_in_every_root_order() {
    let functions = monotone_functions();
    assert_eq!(functions.len(), 20);

    for &function in &functions {
        let system = MonotoneSystem {
            functions: [function; 3],
        };
        let clauses = system.clauses(0);
        for variables in 0_u8..8 {
            assert_eq!(
                clauses_hold(&clauses, variables),
                function_holds(function, variables),
                "function={function:08b}, variables={variables:03b}",
            );
        }
    }

    let mut system_count = 0;
    for &function_a in &functions {
        for &function_b in &functions {
            for &function_c in &functions {
                system_count += 1;
                let system = MonotoneSystem {
                    functions: [function_a, function_b, function_c],
                };

                let expected = {
                    let _mode = memo::TestModeGuard::new(false);
                    BOOLEAN_NODES.map(|node| monotone_node(&system, node).is_proven())
                };

                for order in BOOLEAN_ROOT_ORDERS {
                    let _mode = memo::TestModeGuard::new(true);
                    for node in order {
                        assert_eq!(
                            monotone_node(&system, node).is_proven(),
                            expected[usize::from(node)],
                            "system={system:?}, order={order:?}, node={node}",
                        );
                    }
                }
            }
        }
    }

    assert_eq!(system_count, 8_000);
}
