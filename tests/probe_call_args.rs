//! Probes for conflicting borrows inside one call's arguments.
//! rustc rejects all of these; `borrowck_ok()` passing means formality accepts them.

use a_mir_formality::{crates, BorrowCheckFailure, FormalityTest};
use formality_core::test;

// rustc: E0499 (two mutable borrows of `v` in one call)
#[test]
fn probe_two_mut_borrows_in_call_args() {
    FormalityTest::new(crates![crate Foo {
        fn both<'a, 'b>(x: &'a mut i32, y: &'b mut i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                both::<'r0, 'r1>(&'r0 mut v, &'r1 mut v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0502 (shared borrow `r` is still used by the call while `&mut v` is taken)
#[test]
fn probe_mut_borrow_while_shared_borrow_used_later_arg() {
    FormalityTest::new(crates![crate Foo {
        fn pair<'a, 'b>(x: &'a mut i32, y: &'b i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                let r: &'r0 i32 = &'r0 v;
                pair::<'r1, 'r0>(&'r1 mut v, r);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0499 (two mutable borrows of `v` in one struct expression)
#[test]
fn probe_two_mut_borrows_in_struct_fields() {
    FormalityTest::new(crates![crate Foo {
        struct Pair<'a, 'b> {
            a: &'a mut i32,
            b: &'b mut i32,
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                let p: Pair<'r0, 'r1> = Pair::<'r0, 'r1> { a: &'r0 mut v, b: &'r1 mut v };
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// Control: the same two borrows stored in locals first. rustc: E0499.
#[test]
fn control_two_mut_borrows_in_locals() {
    FormalityTest::new(crates![crate Foo {
        fn both<'a, 'b>(x: &'a mut i32, y: &'b mut i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                let a: &'r0 mut i32 = &'r0 mut v;
                let b: &'r1 mut i32 = &'r1 mut v;
                both::<'r0, 'r1>(a, b);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_err(
        BorrowCheckFailure::All,
        expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = v : i32
            &access.place = v : i32

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_1}
            &lifetime.upcast() = ?lt_1

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`"#]],
    );
}

// rustc: E0502 (&mut v then &v inside one call)
#[test]
fn probe_mut_then_shared_in_one_call() {
    FormalityTest::new(crates![crate Foo {
        fn ms<'a, 'b>(x: &'a mut i32, y: &'b i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                ms::<'r0, 'r1>(&'r0 mut v, &'r1 v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0502 (&v then &mut v inside one call)
#[test]
fn probe_shared_then_mut_in_one_call() {
    FormalityTest::new(crates![crate Foo {
        fn sm<'a, 'b>(x: &'a i32, y: &'b mut i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                sm::<'r0, 'r1>(&'r0 v, &'r1 mut v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0499 (the result of `inner` keeps the first &mut alive)
#[test]
fn probe_nested_call_keeps_first_borrow_alive() {
    FormalityTest::new(crates![crate Foo {
        fn inner<'a>(x: &'a mut i32) -> &'a mut i32 {
            return x;
        }
        fn both<'a, 'b>(x: &'a mut i32, y: &'b mut i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                both::<'r0, 'r1>(inner::<'r0>(&'r0 mut v), &'r1 mut v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// Positive control: rustc accepts two shared borrows in one call.
#[test]
fn control_two_shared_borrows_in_one_call() {
    FormalityTest::new(crates![crate Foo {
        fn two<'a, 'b>(x: &'a i32, y: &'b i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let v: i32 = 0_i32;
                two::<'r0, 'r1>(&'r0 v, &'r1 v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0503 (`v` is read while `&mut v` is live, within one call)
#[test]
fn probe_read_after_mut_borrow_in_one_call() {
    FormalityTest::new(crates![crate Foo {
        fn mu<'a>(x: &'a mut i32, y: i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0> {
                let v: i32 = 0_i32;
                mu::<'r0>(&'r0 mut v, v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0499 (the third argument conflicts with the first, with an unrelated one between)
#[test]
fn probe_three_args_first_and_third_conflict() {
    FormalityTest::new(crates![crate Foo {
        fn three<'a, 'b, 'c>(x: &'a mut i32, y: &'b mut i32, z: &'c mut i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1, 'r2> {
                let v: i32 = 0_i32;
                let w: i32 = 0_i32;
                three::<'r0, 'r1, 'r2>(&'r0 mut v, &'r1 mut w, &'r2 mut v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// Control, rustc accepts: `v` is copied before `&mut v` is taken.
#[test]
fn control_copy_then_mut_in_one_call() {
    FormalityTest::new(crates![crate Foo {
        fn um<'a>(x: i32, y: &'a mut i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0> {
                let v: i32 = 0_i32;
                um::<'r0>(v, &'r0 mut v);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// Control, rustc accepts: disjoint fields can be borrowed mutably together.
#[test]
fn control_disjoint_fields_in_one_call() {
    FormalityTest::new(crates![crate Foo {
        struct S {
            a: i32,
            b: i32,
        }
        fn both<'a, 'b>(x: &'a mut i32, y: &'b mut i32) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0, 'r1> {
                let s: S = S { a: 0_i32, b: 0_i32 };
                both::<'r0, 'r1>(&'r0 mut s.a, &'r1 mut s.b);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0506 (assignment to `v` while `&mut v` is live, within one call)
#[test]
fn probe_write_in_later_arg() {
    FormalityTest::new(crates![crate Foo {
        fn fu<'a>(x: &'a mut u32, y: ()) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0> {
                let v: u32 = 0_u32;
                fu::<'r0>(&'r0 mut v, v = 1_u32);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}

// rustc: E0505 (move out of `s` while `&mut s` is live, within one call)
#[test]
fn probe_move_in_later_arg() {
    FormalityTest::new(crates![crate Foo {
        struct S {}
        fn take_s<'a>(x: &'a mut S, y: S) -> u32 {
            return 0_u32;
        }
        fn foo() -> u32 {
            exists<'r0> {
                let s: S = S {};
                take_s::<'r0>(&'r0 mut s, s);
                return 0_u32;
            }
        }
    }])
    .skip_execute()
    .borrowck_ok();
}
