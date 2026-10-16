#![cfg(test)]

use super::{ProofTree, ProvenSet, Scope};
use crate::{bail, cast_impl, judgment_fn, Fallible};
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

/// Entering a scope can fail, like any other condition's expression.
fn raise_even(num: &Num, offset: u32) -> Fallible<(Raised, Num)> {
    if num.0 % 2 != 0 {
        bail!("{num:?} is odd");
    }
    Ok(raise(num, offset))
}

judgment_fn! {
    /// The successors of an even `num`, computed in a scope raised by 10.
    fn even_raised_successors(
        num: Num,
    ) => Num {
        debug(num)

        (
            (scope(raise_even(num, 10)? => num) with(next)
                (next in successors(num)))
            --------------------------------------- ("successors")
            (even_raised_successors(num) => next)
        )
    }
}

#[test]
fn test_scope_entered() {
    even_raised_successors(Num(20)).assert_ok(expect_test::expect!["{Num(21), Num(22)}"]);
}

/// If the scope cannot be entered, the rule does not apply.
#[test]
fn test_scope_not_entered() {
    even_raised_successors(Num(21)).assert_err(expect_test::expect![[r#"
        the rule "successors" at (test_scope.rs) failed because
          Num(21) is odd"#]]);
}

/// A scope with more than one way out: `leave` proves one for each route.
struct Routes {
    routes: Vec<u32>,
}

/// Enter the scope: inside is `num`, raised by 100.
fn by_routes(num: &Num, routes: Vec<u32>) -> (Routes, Num) {
    (Routes { routes }, Num(num.0 + 100))
}

impl Scope<(Num,)> for Routes {
    fn leave(&self, (n,): (Num,)) -> ProvenSet<(Num,)> {
        self.routes
            .iter()
            .map(|r| ((Num(n.0 - r),), ProofTree::leaf("route")))
            .collect()
    }
}

judgment_fn! {
    /// Each successor of `num` by each route out of the scope.
    fn routed_successors(
        num: Num,
    ) => Num {
        debug(num)

        (
            (scope(by_routes(num, vec![100, 90]) => num) with(next)
                (next in successors(num)))
            --------------------------------------- ("successors")
            (routed_successors(num) => next)
        )
    }
}

/// Every proof of the nested conditions leaves by every route, so one proof
/// inside becomes several outside: successors 101 and 102, less 100 or 90.
#[test]
fn test_scope_leave_proves_several() {
    routed_successors(Num(0)).assert_ok(expect_test::expect!["{Num(1), Num(2), Num(11), Num(12)}"]);
}

/// Nothing proven inside is taken out.
impl Scope<()> for Raised {
    fn leave(&self, (): ()) -> ProvenSet<()> {
        ProvenSet::singleton(((), ProofTree::leaf("nothing to lower")))
    }
}

judgment_fn! {
    /// Whether `num` has a successor above `2 * offset`, proven in the scope
    /// but not taken out of it.
    fn has_high_successor(
        num: Num,
    ) => Num {
        debug(num)

        (
            (scope(raise(num, 10) => raised) with()
                (next in successors(raised))
                (if next.0 > 20))
            --------------------------------------- ("high successor")
            (has_high_successor(num) => num)
        )
    }
}

/// The scope leaves with nothing, and the rule yields its own input.
#[test]
fn test_scope_with_no_variables() {
    has_high_successor(Num(11)).assert_ok(expect_test::expect!["{Num(11)}"]);
}

/// Inside, `num` is 10; neither 11 nor 12 is above 20, so nothing is proven
/// in the scope and the rule fails.
#[test]
fn test_scope_with_no_variables_unproven() {
    has_high_successor(Num(0)).assert_err(expect_test::expect![[r#"
        the rule "high successor" at (test_scope.rs) failed because
          condition evaluated to false: `next.0 > 20`"#]]);
}

judgment_fn! {
    /// A scope nested in a scope, each with a variable coming out of it.
    fn twice_raised_successors(
        num: Num,
    ) => Num {
        debug(num)

        (
            (scope(raise(num, 10) => num) with(next)
                (scope(raise(num, 10) => num) with(next)
                    (next in successors(num))))
            --------------------------------------- ("successors")
            (twice_raised_successors(num) => next)
        )
    }
}

/// Inside both scopes `num` is 40; each successor leaves the inner scope and
/// then the outer one, less 10 each time.
#[test]
fn test_scope_nested_with_variables() {
    twice_raised_successors(Num(20)).assert_ok(expect_test::expect!["{Num(21), Num(22)}"]);
}
