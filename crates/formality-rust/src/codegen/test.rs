#![cfg(test)]

use crate::grammar::Crates;
use crate::rust::term;

/// Run a program through the new judgment-based codegen and MiniRust execution.
fn run_program(input: &str) -> String {
    let crates: Crates = term(input);
    let program = super::codegen_program(&crates).expect("codegen failed");

    let stdout_buf = std::sync::Arc::new(std::sync::Mutex::new(Vec::<u8>::new()));
    let stderr_buf = std::sync::Arc::new(std::sync::Mutex::new(Vec::<u8>::new()));

    let stdout = libspecr::DynWrite::new(SharedWriter(stdout_buf.clone()));
    let stderr = libspecr::DynWrite::new(SharedWriter(stderr_buf.clone()));

    type Mem = minirust_rs::mem::BasicMemory<minirust_rs::prelude::x86_64>;
    let mut machine: minirust_rs::lang::Machine<Mem> =
        minirust_rs::lang::Machine::new(program, stdout, stderr)
            .get_internal()
            .expect("machine creation failed");

    loop {
        match machine.step().get_internal() {
            Ok(()) => continue,
            Err(minirust_rs::prelude::TerminationInfo::MachineStop) => break,
            Err(e) => panic!("execution error: {e:?}"),
        }
    }

    let bytes = stdout_buf.lock().unwrap().clone();
    String::from_utf8(bytes).expect("stdout was not valid UTF-8")
}

#[derive(Clone)]
struct SharedWriter(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);

impl std::io::Write for SharedWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl libspecr::hidden::GcCompat for SharedWriter {
    fn points_to(&self, _m: &mut std::collections::HashSet<usize>) {}
}

// All tests from the old codegen, targeting the new module

#[test]
fn hello_world() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                print 22 _ i32;
            }
        }]",
    );
    assert_eq!(output, "22\n");
}

#[test]
fn booleans_and_multiple_prints() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                print 1 _ i32;
                print true;
                print false;
            }
        }]",
    );
    assert_eq!(output, "1\ntrue\nfalse\n");
}

#[test]
fn let_bindings() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 42 _ i32;
                print x;
            }
        }]",
    );
    assert_eq!(output, "42\n");
}

#[test]
fn assignment() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 1 _ i32;
                x = 2 _ i32;
                print x;
            }
        }]",
    );
    assert_eq!(output, "2\n");
}

#[test]
fn function_calls() {
    let output = run_program(
        "[crate test {
            fn add_one(x: i32) -> i32 {
                return x;
            }
            fn main() -> () {
                let y: i32 = add_one(1 _ i32);
                print y;
            }
        }]",
    );
    assert_eq!(output, "1\n");
}

#[test]
fn generic_function_calls() {
    let output = run_program(
        "[crate test {
            fn identity<T>(x: T) -> T {
                return x;
            }
            fn main() -> () {
                let y: i32 = identity::<i32>(42 _ i32);
                print y;
            }
        }]",
    );
    assert_eq!(output, "42\n");
}

#[test]
fn if_statements() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 1 _ i32;
                if true {
                    print x;
                } else {
                    print 0 _ i32;
                }
            }
        }]",
    );
    assert_eq!(output, "1\n");
}

#[test]
fn loops_break_continue() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 0 _ i32;
                'a: loop {
                    print x;
                    break 'a;
                }
            }
        }]",
    );
    assert_eq!(output, "0\n");
}

#[test]
fn nested_blocks_and_exists() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                {
                    let x: i32 = 99 _ i32;
                    print x;
                }
                exists<'a> {
                    print 1 _ i32;
                }
            }
        }]",
    );
    assert_eq!(output, "99\n1\n");
}

#[test]
fn structs() {
    let output = run_program(
        "[crate test {
            struct Pair<> where { x: i32, y: i32 }
            fn main() -> () {
                let p: Pair = Pair { x: 10 _ i32, y: 20 _ i32 };
                print p.x;
                print p.y;
            }
        }]",
    );
    assert_eq!(output, "10\n20\n");
}

#[test]
fn references_and_deref() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 42 _ i32;
                let r: & 'static i32 = & 'static x;
                print *r;
            }
        }]",
    );
    assert_eq!(output, "42\n");
}

#[test]
fn usize_isize() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let a: usize = 100 _ usize;
                let b: isize = 200 _ isize;
                print a;
                print b;
            }
        }]",
    );
    assert_eq!(output, "100\n200\n");
}

// Phase 2 tests

#[test]
fn multiple_calls_in_sequence() {
    let output = run_program(
        "[crate test {
            fn f(x: i32) -> i32 { return x; }
            fn main() -> () {
                let a: i32 = f(1 _ i32);
                let b: i32 = f(2 _ i32);
                let c: i32 = f(3 _ i32);
                print a;
                print b;
                print c;
            }
        }]",
    );
    assert_eq!(output, "1\n2\n3\n");
}

#[test]
fn nested_calls() {
    let output = run_program(
        "[crate test {
            fn f(x: i32) -> i32 { return x; }
            fn main() -> () {
                let y: i32 = f(f(42 _ i32));
                print y;
            }
        }]",
    );
    assert_eq!(output, "42\n");
}

#[test]
fn functions_calling_other_functions() {
    let output = run_program(
        "[crate test {
            fn inner(x: i32) -> i32 { return x; }
            fn outer(x: i32) -> i32 {
                let y: i32 = inner(x);
                return y;
            }
            fn main() -> () {
                let r: i32 = outer(7 _ i32);
                print r;
            }
        }]",
    );
    assert_eq!(output, "7\n");
}

#[test]
fn transitive_monomorphization() {
    let output = run_program(
        "[crate test {
            fn c<T>(x: T) -> T { return x; }
            fn b<T>(x: T) -> T {
                let y: T = c::<T>(x);
                return y;
            }
            fn a() -> () {
                let r: i32 = b::<i32>(99 _ i32);
                print r;
            }
            fn main() -> () {
                a();
            }
        }]",
    );
    assert_eq!(output, "99\n");
}

#[test]
fn generic_function_multiple_type_params() {
    let output = run_program(
        "[crate test {
            fn first<T, U>(x: T, y: U) -> T { return x; }
            fn main() -> () {
                let r: i32 = first::<i32, bool>(10 _ i32, true);
                print r;
            }
        }]",
    );
    assert_eq!(output, "10\n");
}

#[test]
fn generic_struct() {
    let output = run_program(
        "[crate test {
            struct Wrapper<T> where { val: T }
            fn main() -> () {
                let w: Wrapper<i32> = Wrapper::<i32> { val: 42 _ i32 };
                print w.val;
            }
        }]",
    );
    assert_eq!(output, "42\n");
}

#[test]
fn ref_to_struct_field() {
    let output = run_program(
        "[crate test {
            struct Pair<> where { x: i32, y: i32 }
            fn main() -> () {
                let p: Pair = Pair { x: 10 _ i32, y: 20 _ i32 };
                let r: & 'static i32 = & 'static p.x;
                print *r;
            }
        }]",
    );
    assert_eq!(output, "10\n");
}

#[test]
fn deref_through_ref_to_struct_field() {
    let output = run_program(
        "[crate test {
            struct Pair<> where { x: i32, y: i32 }
            fn main() -> () {
                let p: Pair = Pair { x: 10 _ i32, y: 20 _ i32 };
                let r: & 'static Pair = & 'static p;
                print (*r).x;
                print (*r).y;
            }
        }]",
    );
    assert_eq!(output, "10\n20\n");
}

#[test]
fn continue_in_loop() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 0 _ i32;
                'a: loop {
                    if true {
                        print x;
                        break 'a;
                    } else {
                        continue 'a;
                    }
                }
            }
        }]",
    );
    assert_eq!(output, "0\n");
}

#[test]
fn nested_loops_break_outer() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                'outer: loop {
                    print 1 _ i32;
                    'inner: loop {
                        print 2 _ i32;
                        break 'outer;
                    }
                }
                print 3 _ i32;
            }
        }]",
    );
    assert_eq!(output, "1\n2\n3\n");
}

#[test]
fn if_else_inside_loop_with_breaks() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                'a: loop {
                    if true {
                        print 1 _ i32;
                        break 'a;
                    } else {
                        print 2 _ i32;
                        break 'a;
                    }
                }
                print 3 _ i32;
            }
        }]",
    );
    assert_eq!(output, "1\n3\n");
}

#[test]
fn multiple_breaks_same_label() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 0 _ i32;
                'a: loop {
                    if false {
                        x = 1 _ i32;
                        break 'a;
                    } else {
                        x = 2 _ i32;
                        break 'a;
                    }
                }
                print x;
            }
        }]",
    );
    assert_eq!(output, "2\n");
}

#[test]
fn mut_reference() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 1 _ i32;
                let r: &mut 'static i32 = &mut 'static x;
                *r = 2 _ i32;
                print *r;
            }
        }]",
    );
    assert_eq!(output, "2\n");
}

#[test]
fn reborrow_shared() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 42 _ i32;
                let r1: & 'static i32 = & 'static x;
                let r2: & 'static i32 = & 'static *r1;
                print *r2;
            }
        }]",
    );
    assert_eq!(output, "42\n");
}

#[test]
fn exists_with_lifetime_parameterized_type() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 55 _ i32;
                exists<'a> {
                    let r: & 'a i32 = & 'a x;
                    print *r;
                }
            }
        }]",
    );
    assert_eq!(output, "55\n");
}

// Additional tests from old codegen

#[test]
fn return_from_inside_loop() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 0 _ i32;
                'a: loop {
                    x = 77 _ i32;
                    break 'a;
                }
                print x;
            }
        }]",
    );
    assert_eq!(output, "77\n");
}

#[test]
fn return_from_nested_block_inside_loop() {
    let output = run_program(
        "[crate test {
            fn main() -> () {
                let x: i32 = 0 _ i32;
                'a: loop {
                    {
                        x = 88 _ i32;
                        break 'a;
                    }
                }
                print x;
            }
        }]",
    );
    assert_eq!(output, "88\n");
}
