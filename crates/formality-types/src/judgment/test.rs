#![cfg(test)]

use std::{cell::RefCell, sync::Arc, thread::LocalKey};

use formality_macros::term;

use crate::matcher::Matcher;

use super::{stack::JudgmentStack, Judgment, JudgmentBuilder};

#[term($edges)]
struct Graph {
    edges: Vec<(u32, u32)>,
}

impl Graph {
    fn successors(&self, n: u32) -> impl Iterator<Item = u32> + '_ {
        self.edges
            .iter()
            .flat_map(move |(a, b)| if *a == n { Some(*b) } else { None })
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
        builder.push_rule(|TransitiveReachability(graph, start)| {
            graph.successors(start).collect::<Vec<_>>()
        });

        // `reachable(start, c) :- reachable(start, b), reachable(b, c)`
        builder.push_rule(|TransitiveReachability(graph, start)| {
            TransitiveReachability(graph.clone(), start)
                .into_iter()
                .flat_map(move |b| TransitiveReachability(graph.clone(), b).apply())
        });
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
    "#]].assert_debug_eq(&TransitiveReachability(graph, 0).apply());
}
