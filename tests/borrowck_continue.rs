use a_mir_formality::{crates, BorrowCheckFailure, FormalityTest};

#[test]
fn move_survives_continue() {
    FormalityTest::new(crates![crate Foo {
        struct S { value: u32 }
        fn take(s: S) -> () {}
        fn main() -> () {
            let s: S = S { value: 1_u32 };
            'outer: loop { take(s); continue 'outer; }
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0382]: use of moved value: `s`
          --> lib.rs
           |
         8 |     let mut s: S = S { value: 1_u32 };
           |         ----- move occurs because `s` has type `S`, which does not implement the `Copy` trait
         9 |     'outer: loop {
           |             ---- inside of this loop
        10 |         take(s);
           |              ^ value moved here, in previous iteration of loop
           |
        note: consider changing this parameter type in function `take` to borrow instead if owning the value isn't necessary
          --> lib.rs
           |
         5 | pub fn take(mut s: S) -> () {}
           |        ----        ^ this parameter takes ownership of the value
           |        |
           |        in this function
        note: if `S` implemented `Clone`, you could clone the value
          --> lib.rs
           |
         1 | pub struct S {
           | ^^^^^^^^^^^^ consider implementing `Clone` for this type
        ...
        10 |         take(s);
           |              - you could clone this value

        For more information about this error, try `rustc --explain E0382`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .borrowck_err(BorrowCheckFailure::All, expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [(s, S)], [s : S]), scope(none, Some('outer), {}, Some({s}), [], [])], point_flow_state({}, {}, {}), {}, {}, {})
            state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [(s, S)], [s : S]), scope(none, Some('outer), {}, Some({s}), [], [])], point_flow_state({}, {}, {s}), {}, {}, {})

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]]);
}

#[test]
fn move_survives_continue_to_outer_loop() {
    FormalityTest::new(crates![crate Foo {
        struct S { value: u32 }
        fn take(s: S) -> () {}
        fn main() -> () {
            let s: S = S { value: 1_u32 };
            'outer: loop {
                'inner: loop { take(s); continue 'outer; }
            }
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0382]: use of moved value: `s`
          --> lib.rs
           |
         8 |     let mut s: S = S { value: 1_u32 };
           |         ----- move occurs because `s` has type `S`, which does not implement the `Copy` trait
         9 |     'outer: loop {
           |             ---- inside of this loop
        10 |         'inner: loop {
           |                 ---- inside of this loop
        11 |             take(s);
           |                  ^ value moved here, in previous iteration of loop
           |
        note: consider changing this parameter type in function `take` to borrow instead if owning the value isn't necessary
          --> lib.rs
           |
         5 | pub fn take(mut s: S) -> () {}
           |        ----        ^ this parameter takes ownership of the value
           |        |
           |        in this function
        note: verify that your loop breaking logic is correct
          --> lib.rs
           |
         9 |     'outer: loop {
           |     ------------
        10 |         'inner: loop {
           |         ------------
        11 |             take(s);
        12 |             continue 'outer;
           |             ^^^^^^^^^^^^^^^ this `continue` advances the loop at line 9
        note: if `S` implemented `Clone`, you could clone the value
          --> lib.rs
           |
         1 | pub struct S {
           | ^^^^^^^^^^^^ consider implementing `Clone` for this type
        ...
        11 |             take(s);
           |                  - you could clone this value

        For more information about this error, try `rustc --explain E0382`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .borrowck_err(BorrowCheckFailure::All, expect_test::expect![[r#"
        the rule "fixed-point" at (nll.rs) failed because
          condition evaluated to false: `state0 == state1`
            state0 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [(s, S)], [s : S]), scope(none, Some('outer), {}, Some({s}), [], [])], point_flow_state({}, {}, {}), {}, {}, {})
            state1 = flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [(s, S)], [s : S]), scope(none, Some('outer), {}, Some({s}), [], [])], point_flow_state({}, {}, {s}), {}, {}, {})

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`

        the rule "access_permitted" at (nll.rs) failed because
          condition evaluated to false: `match access.kind
          {
              AccessKind::Write =>
              check_place_writable(&state, &access.place.to_place_expression()),
              AccessKind::Read | AccessKind::Move =>
              check_place_initialized(&state, &access.place.to_place_expression()),
          }`"#]]);
}

#[test]
fn loan_carried_by_continue_remains_live_after_break() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let v: u32 = 0_u32;
            let w: u32 = 2_u32;
            let first: bool = true;
            exists<'b> {
                let q: &'b u32 = &'b w;
                'again: loop {
                    if first {
                        first = false;
                        q = &'b v;
                        continue 'again;
                    } else { break 'again; }
                }
                v = 1_u32;
                println!(*q);
            }
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0506]: cannot assign to `v` because it is borrowed
          --> lib.rs
           |
        10 |                 q = &v;
           |                     -- `v` is borrowed here
        ...
        16 |         v = 1_u32;
           |         ^^^^^^^^^ `v` is assigned to here but it was already borrowed
        17 |         *q;
           |         -- borrow later used here

        For more information about this error, try `rustc --explain E0506`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .borrowck_err(
        BorrowCheckFailure::All,
        expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = v : u32
            &access.place = v : u32

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_1}
            &lifetime.upcast() = ?lt_1

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = v : u32
            &access.place = v : u32

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_1}
            &lifetime.upcast() = ?lt_1

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`"#]],
    );
}

#[test]
fn continue_to_outer_loop_executes() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let first: bool = true;
            'outer: loop {
                if first {
                    first = false;
                    'inner: loop { continue 'outer; }
                } else { break 'outer; }
            }
            println!(7_u32);
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .borrowck_ok();
}

#[test]
fn only_break_carries_initialized_value_out_of_loop() {
    FormalityTest::new(crates![crate Foo {
        struct S { value: u32 }
        fn take(s: S) -> () {}
        fn main() -> () {
            let s: S;
            let first: bool = true;
            'again: loop {
                s = S { value: 7_u32 };
                if first {
                    first = false;
                    take(s);
                    continue 'again;
                } else {
                    break 'again;
                }
            }
            println!(s.value);
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .borrowck_ok();
}
