use a_mir_formality::{crates, FormalityTest};

#[test]
#[should_panic(expected = "execution step limit reached after 5 steps")]
fn execution_limit_stops_an_endless_loop() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            'a: loop {}
        }
    }])
    .execution_step_limit(5)
    .ok();
}

#[test]
fn execution_limit_finishes_within_allowance() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .execution_step_limit(20)
    .expect_output("1\n")
    .ok();
}

#[test]
fn execution_limit_finishes_on_last_allowed_step() {
    // This program currently takes 11 MiniRust steps, including MachineStop.
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .execution_step_limit(11)
    .expect_output("1\n")
    .ok();
}

#[test]
#[should_panic(expected = "execution step limit reached after 10 steps")]
fn execution_limit_stops_when_allowance_is_too_small() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .execution_step_limit(10)
    .ok();
}

#[test]
fn execution_limit_skipped_execution_needs_no_steps() {
    let program = crates![crate Foo {
        fn example() -> () {}
    }];

    FormalityTest::new(program)
        .execution_step_limit(0)
        .skip_execute()
        .ok();
}

#[test]
fn execution_limit_default_allowance_runs_small_programs() {
    let program = crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }];
    FormalityTest::new(program).expect_output("1\n").ok();
}

#[test]
#[should_panic(expected = "execution step limit reached after 0 steps")]
fn execution_limit_zero_allowance_prevents_execution() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .execution_step_limit(0)
    .ok();
}

#[test]
#[should_panic(expected = "program output mismatch")]
fn ok_checks_output_expectation() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () { println!(1_u32); }
    }])
    .expect_output("999\n")
    .ok();
}

#[test]
fn ok_skipped_execution_does_not_check_output() {
    let program = crates![crate Foo {
        fn example() -> () {}
    }];

    FormalityTest::new(program)
        .expect_output("999\n")
        .skip_execute()
        .ok();
}

#[test]
fn hello_world() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            println!(22_i32);
        }
    }])
    .expect_output("22\n")
    .ok()
}

#[test]
fn booleans_and_multiple_prints() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            println!(1_i32);
            println!(true);
            println!(false);
        }
    }])
    .expect_output("1\ntrue\nfalse\n")
    .ok()
}

#[test]
fn let_bindings() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 42_i32;
            println!(x);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn assignment() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1_i32;
            x = 2 _ i32;
            println!(x);
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
fn function_calls() {
    FormalityTest::new(crates![crate test {
        fn add_one(x: i32) -> i32 {
            return x; // FIXME: this should probably be `x + 1`
        }
        fn main() -> () {
            let y: i32 = add_one(1_i32);
            println!(y);
        }
    }])
    .expect_output("1\n")
    .ok()
}

#[test]
fn generic_function_calls() {
    FormalityTest::new(crates![crate test {
        fn identity<T>(x: T) -> T {
            return x;
        }
        fn main() -> () {
            let y: i32 = identity::<i32>(42_i32);
            println!(y);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn if_statements() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1_i32;
            if true {
                println!(x);
            } else {
                println!(0_i32);
            }
        }
    }])
    .expect_output("1\n")
    .ok()
}

#[test]
fn loops_break_continue() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0_i32;
            'a: loop {
                println!(x);
                break 'a;
            }
        }
    }])
    .expect_output("0\n")
    .ok()
}

#[test]
fn nested_blocks_and_exists() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            {
                let x: i32 = 99_i32;
                println!(x);
            }
            exists<'a> {
                println!(1_i32);
            }
        }
    }])
    .expect_output("99\n1\n")
    .ok()
}

#[test]
fn structs() {
    FormalityTest::new(crates![crate test {
        struct Pair<> where { x: i32, y: i32 }
        fn main() -> () {
            let p: Pair = Pair { x: 10_i32, y: 20_i32 };
            println!(p.x);
            println!(p.y);
        }
    }])
    .expect_output("10\n20\n")
    .ok()
}

#[test]
fn references_and_deref() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 42_i32;
            exists<'a> {
                let r: & 'a i32 = & 'a x;
                println!(*r);
            }
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn usize_isize() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let a: usize = 100_usize;
            let b: isize = 200_isize;
            println!(a);
            println!(b);
        }
    }])
    .expect_output("100\n200\n")
    .ok()
}

#[test]
fn multiple_calls_in_sequence() {
    FormalityTest::new(crates![crate test {
        fn f(x: i32) -> i32 { return x; }
        fn main() -> () {
            let a: i32 = f(1_i32);
            let b: i32 = f(2_i32);
            let c: i32 = f(3_i32);
            println!(a);
            println!(b);
            println!(c);
        }
    }])
    .expect_output("1\n2\n3\n")
    .ok()
}

#[test]
fn nested_calls() {
    FormalityTest::new(crates![crate test {
        fn f(x: i32) -> i32 { return x; }
        fn main() -> () {
            let y: i32 = f(f(42_i32));
            println!(y);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn functions_calling_other_functions() {
    FormalityTest::new(crates![crate test {
        fn inner(x: i32) -> i32 { return x; }
        fn outer(x: i32) -> i32 {
            let y: i32 = inner(x);
            return y;
        }
        fn main() -> () {
            let r: i32 = outer(7_i32);
            println!(r);
        }
    }])
    .expect_output("7\n")
    .ok()
}

#[test]
fn transitive_monomorphization() {
    FormalityTest::new(crates![crate test {
        fn c<T>(x: T) -> T { return x; }
        fn b<T>(x: T) -> T {
            let y: T = c::<T>(x);
            return y;
        }
        fn a() -> () {
            let r: i32 = b::<i32>(99_i32);
            println!(r);
        }
        fn main() -> () {
            a();
        }
    }])
    .expect_output("99\n")
    .ok()
}

#[test]
fn generic_function_multiple_type_params() {
    FormalityTest::new(crates![crate test {
        fn first<T, U>(x: T, y: U) -> T { return x; }
        fn main() -> () {
            let r: i32 = first::<i32, bool>(10_i32, true);
            println!(r);
        }
    }])
    .expect_output("10\n")
    .ok()
}

#[test]
fn generic_struct() {
    FormalityTest::new(crates![crate test {
        struct Wrapper<T> where { val: T }
        fn main() -> () {
            let w: Wrapper<i32> = Wrapper::<i32> { val: 42_i32 };
            println!(w.val);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn ref_to_struct_field() {
    FormalityTest::new(crates![crate test {
        struct Pair<> where { x: i32, y: i32 }
        fn main() -> () {
            let p: Pair = Pair { x: 10_i32, y: 20_i32 };
            exists<'a> {
                let r: & 'a i32 = & 'a p.x;
                println!(*r);
            }
        }
    }])
    .expect_output("10\n")
    .ok()
}

#[test]
fn deref_through_ref_to_struct_field() {
    FormalityTest::new(crates![crate test {
        struct Pair<> where { x: i32, y: i32 }
        fn main() -> () {
            let p: Pair = Pair { x: 10_i32, y: 20_i32 };
            exists<'a> {
                let r: & 'a Pair = & 'a p;
                println!((*r).x);
                println!((*r).y);
            }
        }
    }])
    .expect_output("10\n20\n")
    .ok()
}

#[test]
fn continue_in_loop() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0_i32;
            'a: loop {
                if true {
                    println!(x);
                    break 'a;
                } else {
                    continue 'a;
                }
            }
        }
    }])
    .expect_output("0\n")
    .ok()
}

#[test]
fn nested_loops_break_outer() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            'outer: loop {
                println!(1_i32);
                'inner: loop {
                    println!(2_i32);
                    break 'outer;
                }
            }
            println!(3_i32);
        }
    }])
    .expect_output("1\n2\n3\n")
    .ok()
}

#[test]
fn if_else_inside_loop_with_breaks() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            'a: loop {
                if true {
                    println!(1_i32);
                    break 'a;
                } else {
                    println!(2_i32);
                    break 'a;
                }
            }
            println!(3_i32);
        }
    }])
    .expect_output("1\n3\n")
    .ok()
}

#[test]
fn multiple_breaks_same_label() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0_i32;
            'a: loop {
                if false {
                    x = 1_i32;
                    break 'a;
                } else {
                    x = 2_i32;
                    break 'a;
                }
            }
            println!(x);
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
fn mut_reference() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1_i32;
            exists<'a> {
                let r: &'a mut i32 = &'a mut x;
                *r = 2_i32;
                println!(*r);
            }
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
fn reborrow_shared() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 42_i32;
            exists<'a> {
                let r1: & 'a i32 = & 'a x;
                let r2: & 'a i32 = & 'a *r1;
                println!(*r2);
            }
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn exists_with_lifetime_parameterized_type() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 55_i32;
            exists<'a> {
                let r: & 'a i32 = & 'a x;
                println!(*r);
            }
        }
    }])
    .expect_output("55\n")
    .ok()
}

#[test]
fn return_from_inside_loop() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0_i32;
            'a: loop {
                x = 77_i32;
                break 'a;
            }
            println!(x);
        }
    }])
    .expect_output("77\n")
    .ok()
}

#[test]
fn return_from_nested_block_inside_loop() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0_i32;
            'a: loop {
                {
                    x = 88_i32;
                    break 'a;
                }
            }
            println!(x);
        }
    }])
    .expect_output("88\n")
    .ok()
}

#[test]
fn break_from_labeled_block() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            'a: {
                println!(1_i32);
                break 'a;
                println!(2_i32);
            }
            println!(3_i32);
        }
    }])
    .expect_output("1\n3\n")
    .ok()
}

#[test]
fn reference_mutation_then_read() {
    FormalityTest::new(crates![crate Foo {
        fn update<'a>(r: &'a mut u32) -> u32 {
            *r = 10_u32;
            return 2_u32;
        }
        fn main() -> () {
            let x: u32 = 1_u32;
            exists<'a> {
                let r: &'a mut u32 = &'a mut x;
                let amount: u32 = update::<'a>(r);
                println!(amount);
            }
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
fn integer_addition() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1_i32 + 2_i32;
            println!(x);
        }
    }])
    .expect_output("3\n")
    .ok()
}

#[test]
fn addition_in_assignment() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: u32 = 0_u32;
            x = 1_u32 + 2_u32 + 3_u32;
            println!(x);
        }
    }])
    .expect_output("6\n")
    .ok()
}

#[test]
fn associated_type_in_generic_signature() {
    FormalityTest::new(crates![crate Foo {
        trait Value { type Item: []; }
        struct S {}
        impl Value for S { type Item = u32; }
        fn identity<T>(x: <T as Value>::Item) -> <T as Value>::Item where T: Value {
            return x;
        }
        fn main() -> () {
            println!(identity::<S>(7_u32));
        }
    }])
    .expect_output("7\n")
    .ok()
}

#[test]
fn addition_evaluates_calls_left_to_right() {
    FormalityTest::new(crates![crate test {
        fn left() -> i32 { println!(1_i32); return 10_i32; }
        fn right() -> i32 { println!(2_i32); return 20_i32; }
        fn main() -> () { println!(left() + right()); }
    }])
    .rustc_ok()
    .expect_output("1\n2\n30\n")
    .ok()
}

#[test]
fn addition_in_call_arguments() {
    FormalityTest::new(crates![crate test {
        fn sum(x: i32, y: i32) -> i32 { return x + y; }
        fn main() -> () {
            println!(sum(1_i32 + 2_i32, sum(3_i32, 4_i32)));
        }
    }])
    .rustc_ok()
    .expect_output("10\n")
    .ok()
}

#[test]
fn parse_and_run_with_128_bit_integers() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let i_min: i128 = -170141183460469231731687303715884105728 _ i128;
            let i_max: i128 = 170141183460469231731687303715884105727 _ i128;
            let u_min: u128 = 0 _ u128;
            let u_max: u128 = 340282366920938463463374607431768211455 _ u128;
            println!(i_min);
            println!(i_max);
            println!(u_min);
            println!(u_max);
        }
    }])
    .expect_output(format!(
        "{}\n{}\n{}\n{}\n",
        i128::MIN,
        i128::MAX,
        u128::MIN,
        u128::MAX
    ))
    .ok()
}

#[test]
fn field_evaluation_order() {
    FormalityTest::new(crates![crate Foo {
        struct S { a: u32, b: u32 }
        fn first() -> u32 { println!(1_u32); return 10_u32; }
        fn second() -> u32 { println!(2_u32); return 20_u32; }
        fn main() -> () {
            let s: S = S { b: second(), a: first() };
            println!(s.a);
            println!(s.b);
        }
    }])
    .expect_output("2\n1\n10\n20\n")
    .ok();
}

#[test]
fn integer_subtraction() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(10_i32 - 3_i32);
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .ok();
}

#[test]
fn associated_type_in_struct_field() {
    FormalityTest::new(crates![crate Foo {
        trait Value { type Item: []; }
        struct S {}
        impl Value for S { type Item = u32; }
        struct Wrap<T> { value: T }
        fn main() -> () {
            let w: Wrap<<S as Value>::Item> = Wrap::<<S as Value>::Item> { value: 7_u32 };
            println!(w.value);
        }
    }])
    .expect_output("7\n")
    .ok()
}

#[test]
fn integer_subtraction_negative_result() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(3_i32 - 10_i32);
        }
    }])
    .rustc_ok()
    .expect_output("-7\n")
    .ok();
}

#[test]
fn integer_subtraction_is_left_associative() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(10_i32 - 3_i32 - 2_i32);
        }
    }])
    .rustc_ok()
    .expect_output("5\n")
    .ok();
}

#[test]
fn integer_subtraction_with_addition() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(10_i32 - 3_i32 + 2_i32);
        }
    }])
    .rustc_ok()
    .expect_output("9\n")
    .ok();
}

#[test]
fn subtraction_evaluates_calls_left_to_right() {
    FormalityTest::new(crates![crate Foo {
        fn left() -> i32 { println!(1_i32); return 10_i32; }
        fn right() -> i32 { println!(2_i32); return 3_i32; }
        fn main() -> () { println!(left() - right()); }
    }])
    .rustc_ok()
    .expect_output("1\n2\n7\n")
    .ok();
}

#[test]
fn integer_subtraction_unsigned() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(10_u32 - 3_u32);
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .ok();
}

#[test]
fn chained_associated_types() {
    FormalityTest::new(crates![crate Foo {
        trait Inner { type Item: []; }
        trait Outer { type Item: []; }
        struct S {}
        impl Inner for S { type Item = u32; }
        impl Outer for S { type Item = <S as Inner>::Item; }
        fn main() -> () {
            let x: <S as Outer>::Item = 7_u32;
            println!(x);
        }
    }])
    .expect_output("7\n")
    .ok()
}

#[test]
fn integer_subtraction_with_parentheses() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(10_i32 - (3_i32 - 2_i32));
        }
    }])
    .rustc_ok()
    .expect_output("9\n")
    .ok();
}

#[test]
fn nested_parentheses_preserve_grouping() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(20_i32 - (10_i32 - (3_i32 - 1_i32)));
        }
    }])
    .rustc_ok()
    .expect_output("12\n")
    .ok();
}

#[test]
fn parenthesized_assignment_updates_local() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let x: i32 = 10_i32;
            (x = (x - 3_i32));
            println!(x);
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .ok();
}

// FIXME: Type checking cannot choose between pending-outlives proofs;
// this currently fails before reaching codegen.
#[test]
#[ignore = "type-checking ambiguity between pending-outlives proofs"]
fn nested_reference_dereference() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let x: u32 = 7_u32;
            exists<'a, 'b> {
                let r: &'a u32 = &'a x;
                let rr: &'b &'a u32 = &'b r;
                println!(**rr);
            }
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .ok();
}

// FIXME: Type checking cannot choose between pending-outlives proofs;
// this currently fails before reaching codegen.
#[test]
#[ignore = "type-checking ambiguity between pending-outlives proofs"]
fn associated_type_field_through_reference() {
    FormalityTest::new(crates![crate Foo {
        trait Value { type Item: []; }
        struct S {}
        impl Value for S { type Item = u32; }
        struct Wrap<T> { value: T }
        fn main() -> () {
            let w: Wrap<<S as Value>::Item> = Wrap::<<S as Value>::Item> { value: 7_u32 };
            exists<'a> {
                let r: &'a Wrap<<S as Value>::Item> = &'a w;
                println!((*r).value);
            }
        }
    }])
    .rustc_ok()
    .expect_output("7\n")
    .ok()
}

#[test]
fn integer_multiplication() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(3_i32 * 4_i32);
        }
    }])
    .rustc_ok()
    .expect_output("12\n")
    .ok();
}

#[test]
fn multiplication_before_addition() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(2_i32 + 3_i32 * 4_i32);
            println!(2_i32 * 3_i32 + 4_i32);
            println!(2_i32 + 3_i32 * 4_i32 * 2_i32);
        }
    }])
    .rustc_ok()
    .expect_output("14\n10\n26\n")
    .ok();
}

#[test]
fn associated_type_distinct_instantiations() {
    FormalityTest::new(crates![crate Foo {
        trait Value { type Item: []; }
        struct Number {}
        struct Flag {}
        impl Value for Number { type Item = u32; }
        impl Value for Flag { type Item = bool; }
        fn identity<T>(x: <T as Value>::Item) -> <T as Value>::Item where T: Value {
            return x;
        }
        fn main() -> () {
            println!(identity::<Number>(7_u32));
            println!(identity::<Flag>(true));
            println!(identity::<Number>(9_u32));
        }
    }])
    .rustc_ok()
    .expect_output("7\ntrue\n9\n")
    .ok();
}

/// Exercise codegen's error path directly: ordinary checking rejects this input.
#[test]
fn unresolvable_associated_type_reports_error() {
    let program = formality_rust::rust::term(crates![crate Foo {
        trait Value { type Item: []; }
        struct S {}
        fn main() -> () {
            let x: <S as Value>::Item;
        }
    }]);
    let error = formality_rust::codegen::codegen_program(&program).unwrap_err();
    let message = format!("{error:#}");
    assert!(message.contains("failed to normalize alias"), "{message}");
}

// FIXME: Enable once PR #496 supplies compound-assignment syntax.
// Verified to execute and produce 12 with #488 and #496 combined.
#[test]
#[ignore = "requires compound assignment support from PR #496"]
fn compound_assignment_reads_destination_after_rhs_call() {
    FormalityTest::new(crates![crate Foo {
        fn update<'a>(r: &'a mut u32) -> u32 {
            *r = 10_u32;
            return 2_u32;
        }
        fn main() -> () {
            let x: u32 = 1_u32;
            exists<'a> {
                let r: &'a mut u32 = &'a mut x;
                x += update::<'a>(r);
            }
            println!(x);
        }
    }])
    .rustc_ok()
    .expect_output("12\n")
    .ok()
}

#[test]
fn integer_multiplication_negative_result() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(-3_i32 * 4_i32);
        }
    }])
    .rustc_ok()
    .expect_output("-12\n")
    .ok();
}

#[test]
fn integer_multiplication_unsigned() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(3_u32 * 4_u32);
        }
    }])
    .rustc_ok()
    .expect_output("12\n")
    .ok();
}

#[test]
fn multiplication_runs_calls_left_to_right() {
    FormalityTest::new(crates![crate Foo {
        fn left() -> i32 { println!(1_i32); return 3_i32; }
        fn right() -> i32 { println!(2_i32); return 4_i32; }
        fn main() -> () { println!(left() * right()); }
    }])
    .rustc_ok()
    .expect_output("1\n2\n12\n")
    .ok();
}

#[test]
fn multiplication_with_parentheses() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!((2_i32 + 3_i32) * 4_i32);
            println!(2_i32 * (3_i32 + 4_i32));
        }
    }])
    .rustc_ok()
    .expect_output("20\n14\n")
    .ok();
}

#[test]
fn multiplication_with_subtraction() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            println!(20_i32 - 2_i32 * 3_i32);
            println!((20_i32 - 2_i32) * 3_i32);
        }
    }])
    .rustc_ok()
    .expect_output("14\n54\n")
    .ok();
}

// FIXME(#478): Normalize Derefable::Target before MiniRust type translation.
#[test]
#[ignore = "FIXME(#478): codegen cannot translate Derefable::Target yet"]
fn multiplication_with_dereference() {
    FormalityTest::new(crates![crate Foo {
        fn multiply_left<'a>(r: &'a i32) -> i32 {
            return *r * 3_i32;
        }
        fn multiply_right<'a>(r: &'a i32) -> i32 {
            return 3_i32 * *r;
        }
        fn main() -> () {
            let x: i32 = 4_i32;
            exists<'a, 'b> {
                let r: &'a i32 = &'a x;
                println!(multiply_left::<'a>(r));
                let s: &'b i32 = &'b x;
                println!(multiply_right::<'b>(s));
            }
        }
    }])
    .rustc_ok()
    .expect_output("12\n12\n")
    .ok();
}

// FIXME(#511): Copy/move proof search rejects the second read of *r.
// This also reproduces with addition. Execution additionally needs #478.
#[test]
#[ignore = "FIXME(#511): repeated Copy dereference rejected; codegen also needs #478"]
fn multiplication_reuses_copy_referent() {
    FormalityTest::new(crates![crate Foo {
        fn main() -> () {
            let x: i32 = 4_i32;
            exists<'a> {
                let r: &'a i32 = &'a x;
                println!(*r * 3_i32);
                println!(3_i32 * *r);
            }
        }
    }])
    .rustc_ok()
    .expect_output("12\n12\n")
    .ok();
}
