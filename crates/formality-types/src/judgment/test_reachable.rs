#![cfg(test)]

use std::{cell::RefCell, sync::Arc, thread::LocalKey};

use formality_macros::term;

use crate::cast::Matcher;

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

            // reachable(graph, start, s) :- successor(graph, start, s)
            (TransitiveReachability(graph, start) =
                for s in graph.successors(start),
                yield s
            )

            // reachable(graph, a, c) :- reachable(graph, a, b), reachable(graph, b, c)
            (TransitiveReachability(graph, a) =
                for b in TransitiveReachability(graph.clone(), a),
                for c in TransitiveReachability(graph.clone(), b),
                yield c
            )

        );
    }
}

impl Matcher<TransitiveReachability> for TransitiveReachability {
    fn try_match(term: &TransitiveReachability) -> Option<TransitiveReachability> {
        Some(term.clone())
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
