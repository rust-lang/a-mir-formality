use a_mir_formality::{crates, FormalityTest};

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
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
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
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
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
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
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
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
fn mut_reference() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1_i32;
            exists<'a> {
                let r: &'a mut i32 = &mut 'a x;
                *r = 2_i32;
                println!(*r);
            }
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
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
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
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
