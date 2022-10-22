#![cfg(test)]

use std::{cell::RefCell, sync::Arc, thread::LocalKey};

use formality_macros::term;

use super::{stack::JudgmentStack, Judgment, JudgmentBuilder};

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

#[term(reachable($v0,$v1))]
struct TransitiveReachability(Arc<Graph>, u32);

impl Judgment for TransitiveReachability {
    type Output = u32;

    fn stack() -> &'static LocalKey<RefCell<JudgmentStack<Self>>> {
        thread_local! {
            static R: RefCell<JudgmentStack<TransitiveReachability>> = Default::default()
        }
        &R
    }

    fn build_rules(builder: &mut JudgmentBuilder<Self>) {
        crate::push_rules!(
            builder,

            (
                (graph.successors(start) => s)
                ---------------------------------------
                (TransitiveReachability(graph, start) => s)
            )

            (
                (TransitiveReachability(graph.clone(), a) => b)
                (TransitiveReachability(graph.clone(), b) => c)
                ---------------------------------------
                (TransitiveReachability(graph, a) => c)
            )

        );
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
    .assert_debug_eq(&TransitiveReachability(graph, 0).apply());
}
