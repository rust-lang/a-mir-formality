#![cfg(test)]

use super::{ProofTree, ProvenSet, Scope};
use crate::{cast_impl, judgment_fn};
use formality_macros::test;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
struct Num(u32);

cast_impl!(Num);

/// A scope in which numbers are `offset` higher. Only numbers above
/// `2 * offset` can leave it.
struct Raised {
    offset: u32,
}

/// Enter the scope: inside is `num`, raised.
fn raise(num: &Num, offset: u32) -> (Raised, Num) {
    (Raised { offset }, Num(num.0 + offset))
}

impl Scope<(Num,)> for Raised {
    fn leave(&self, (n,): (Num,)) -> ProvenSet<(Num,)> {
        if n.0 > 2 * self.offset {
            let lowered = Num(n.0 - self.offset);
            ProvenSet::singleton(((lowered,), ProofTree::leaf("lowered")))
        } else {
            ProvenSet::failed(
                "leave",
                crate::judgment::FailureLocation::caller(),
                format!("{n:?} cannot leave a scope raised by {}", self.offset),
            )
        }
    }
}

fn successors(n: &Num) -> Vec<Num> {
    vec![Num(n.0 + 1), Num(n.0 + 2)]
}

judgment_fn! {
    /// The successors of `num`, computed in a scope raised by 10.
    fn raised_successors(
        num: Num,
    ) => Num {
        debug(num)

        (
            (scope(raise(num, 10) => num) with(next)
                (next in successors(num)))
            // `num` is the input again, `next` has left the scope
            (if next.0 > num.0)
            --------------------------------------- ("successors")
            (raised_successors(num) => next)
        )
    }
}

/// Every proof of the nested conditions leaves the scope, and the bindings
/// made inside do not.
#[test]
fn test_scope_each_proof_leaves() {
    raised_successors(Num(20)).assert_ok(expect_test::expect!["{Num(21), Num(22)}"]);
}

/// A proof that cannot leave the scope is not a proof of the rule: inside,
/// `num` is 19, and of its successors only 21 can leave.
#[test]
fn test_scope_some_proofs_cannot_leave() {
    raised_successors(Num(9)).assert_ok(expect_test::expect!["{Num(11)}"]);
}

/// If none can, the rule fails with the reasons.
#[test]
fn test_scope_no_proof_can_leave() {
    raised_successors(Num(1)).assert_err(expect_test::expect![[r#"
        failed at (proven_set.rs) because
          Num(12) cannot leave a scope raised by 10

        failed at (proven_set.rs) because
          Num(13) cannot leave a scope raised by 10"#]]);
}

judgment_fn! {
    /// A scope with several `with` variables and several nested conditions.
    fn raised_pair(
        num: Num,
    ) => (Num, Num) {
        debug(num)

        (
            (scope(raise(num, 10) => num) with(a, b)
                (a in successors(num))
                (let b: Num = Num(a.0 + 100))
                (if a.0 % 2 == 0))
            --------------------------------------- ("pair")
            (raised_pair(num) => (a, b))
        )
    }
}

impl Scope<(Num, Num)> for Raised {
    fn leave(&self, (a, b): (Num, Num)) -> ProvenSet<(Num, Num)> {
        ProvenSet::singleton(((a, b), ProofTree::leaf("as is")))
    }
}

/// Inside, `num` is 11. Of its successors 12 and 13, only 12 is even.
#[test]
fn test_scope_with_two_variables() {
    raised_pair(Num(1)).assert_ok(expect_test::expect!["{(Num(12), Num(112))}"]);
}
