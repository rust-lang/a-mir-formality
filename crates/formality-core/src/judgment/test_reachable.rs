#![cfg(test)]

use crate::{cast_impl, judgment_fn};
use formality_macros::test;
use std::sync::Arc;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
struct Graph {
    edges: Vec<(u32, u32)>,
}

cast_impl!(Graph);

impl Graph {
    fn successors(&self, n: u32) -> Vec<u32> {
        self.edges
            .iter()
            .flat_map(|(a, b)| if *a == n { Some(*b) } else { None })
            .collect()
    }
}

judgment_fn! {
    fn transitive_reachable(
        graph: Arc<Graph>,
        from: u32,
    ) => u32 {
        debug(from, graph)

        (
            (graph.successors(start) => s)
            --------------------------------------- ("base")
            (transitive_reachable(graph, start) => s)
        )

        (
            (transitive_reachable(&graph, a) => b)
            (transitive_reachable(&graph, b) => c)
            --------------------------------------- ("transitive")
            (transitive_reachable(graph, a) => c)
        )
    }
}

#[test]
fn judgment() {
    let graph = Arc::new(Graph {
        edges: vec![(0, 1), (1, 2), (2, 0), (2, 3)],
    });

    expect_test::expect![[r#"
        {
            0,
            1,
            2,
            3,
        }
    "#]]
    .assert_debug_eq(&transitive_reachable(graph, 0));
}
