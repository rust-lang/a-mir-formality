#![cfg(test)]

use std::sync::Arc;

use formality_macros::{term, test};

use crate::{judgment_fn};

#[term($edges)]
struct Graph {
    edges: Vec<(u32, u32)>,
}

impl Graph {
    fn successors(&self, n: u32) -> Vec<u32> {
        self.edges
            .iter()
            .flat_map(|(a, b)| if *a == n { Some(*b) } else { None })
            .collect()
    }
}

judgment_fn!(
    fn transitive_reachable(g: Arc<Graph>, node: u32) => u32 {
        (
            (graph.successors(a) => b)
            (if b % 2 == 0)
            --------------------------------------- ("base")
            (transitive_reachable(graph, a) => b)
        )
    
        (
            (transitive_reachable(&graph, a) => b)
            (transitive_reachable(&graph, b) => c)
            (if c % 2 == 0)
            --------------------------------------- ("transitive")
            (transitive_reachable(graph, a) => c)
        )
    }
);

#[test]
fn judgment() {
    let graph = Arc::new(Graph {
        edges: vec![(0, 1), (1, 2), (2, 4), (2, 3), (3, 6), (4, 8), (8, 10)],
    });

    expect_test::expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&transitive_reachable(&graph, 0));

    expect_test::expect![[r#"
        {
            4,
            8,
            10,
        }
    "#]]
    .assert_debug_eq(&transitive_reachable(&graph, 2));
}
