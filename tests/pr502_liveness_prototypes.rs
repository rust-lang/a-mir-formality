//! Local follow-up prototypes for PR #502.
use a_mir_formality::{crates, BorrowCheckFailure, FormalityTest};
use formality_core::test;

#[test]
fn overwrite_before_later_read() {
    // The model has no value-returning block expressions. A nested call
    // sequences the assignment before the read, like `{ r = &w; r }`.
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { f(); }
        fn after_reset<'a>(unit: (), r: &'a u32) -> &'a u32 { return r; }
        fn pair<'a, 'b>(a: &'a mut u32, b: &'b u32) -> () { println!(*b); }
        fn f() -> () {
            exists<'r0, 'r1> {
                let v: u32 = 0_u32;
                let w: u32 = 1_u32;
                let r: &'r0 u32 = &'r0 v;
                pair::<'r1, 'r0>(&'r1 mut v, after_reset::<'r0>(r = &'r0 w, r));
            }
        }
    }])
    .rustc_ok()
    .expect_output("1\n")
    // FIXME 532: Remove skip_execute once codegen handles borrow-producing expressions.
    .skip_execute()
    .borrowck_ok();
}

#[test]
fn nested_later_read_keeps_loan_live() {
    FormalityTest::new(crates![crate Foo {
        fn identity<'a>(r: &'a u32) -> &'a u32 { return r; }
        fn pair<'a, 'b>(a: &'a mut u32, b: &'b u32) -> () {}
        fn f() -> () {
            exists<'r0, 'r1> {
                let v: u32 = 0_u32;
                let r: &'r0 u32 = &'r0 v;
                pair::<'r1, 'r0>(&'r1 mut v, identity::<'r0>(r));
            }
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0502]: cannot borrow `v` as mutable because it is also borrowed as immutable
          --> lib.rs
           |
        10 |         let mut r: &'_ u32 = &v;
           |                              -- immutable borrow occurs here
        11 |         pair(&mut v, identity(r));
           |              ^^^^^^           - immutable borrow later used here
           |              |
           |              mutable borrow occurs here

        For more information about this error, try `rustc --explain E0502`.
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
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`"#]],
    );
}

#[test]
fn reordered_fields_borrow_before_read_rejected() {
    FormalityTest::new(crates![crate Foo {
        struct Pair<'a> { a: u32, b: &'a mut u32 }
        fn f() -> () {
            exists<'r0, 'r1> {
                let v: u32 = 0_u32;
                let r: &'r0 u32 = &'r0 v;
                let p: Pair<'r1> = Pair::<'r1> { b: &'r1 mut v, a: *r };
            }
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0502]: cannot borrow `v` as mutable because it is also borrowed as immutable
          --> lib.rs
           |
         9 |         let mut r: &'_ u32 = &v;
           |                              -- immutable borrow occurs here
        10 |         let mut p: Pair<'_> = Pair::<'_> { b: &mut v, a: *r };
           |                                               ^^^^^^     -- immutable borrow later used here
           |                                               |
           |                                               mutable borrow occurs here

        For more information about this error, try `rustc --explain E0502`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .borrowck_err(BorrowCheckFailure::All, expect_test::expect![[r#"
        the rule "struct" at (nll.rs) failed because
          condition evaluated to false: `field.name == field_exprs[i].name`

        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = v : u32
            &access.place = v : u32

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_1}
            &lifetime.upcast() = ?lt_1

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`"#]]);
}

#[test]
fn reordered_fields_read_before_borrow_accepted() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { f(); }
        struct Pair<'a> { b: &'a mut u32, a: u32 }
        fn f() -> () {
            exists<'r0, 'r1> {
                let v: u32 = 0_u32;
                let r: &'r0 u32 = &'r0 v;
                let p: Pair<'r1> = Pair::<'r1> { a: *r, b: &'r1 mut v };
                println!(p.a);
            }
        }
    }])
    .rustc_ok()
    .expect_output("0\n")
    .borrowck_ok();
}

#[test]
fn read_after_call_keeps_loan_live() {
    FormalityTest::new(crates![crate Foo {
        fn touch<'a>(v: &'a mut u32) -> () {}
        fn f() -> u32 {
            exists<'r0, 'r1> {
                let v: u32 = 0_u32;
                let r: &'r0 u32 = &'r0 v;
                touch::<'r1>(&'r1 mut v);
                return *r;
            }
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0502]: cannot borrow `v` as mutable because it is also borrowed as immutable
         --> lib.rs
          |
        6 |         let mut r: &'_ u32 = &v;
          |                              -- immutable borrow occurs here
        7 |         touch(&mut v);
          |               ^^^^^^ mutable borrow occurs here
        8 |         return *r;
          |                -- immutable borrow later used here

        For more information about this error, try `rustc --explain E0502`.
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
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`"#]],
    );
}
