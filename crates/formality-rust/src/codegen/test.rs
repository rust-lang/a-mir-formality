#![cfg(test)]

use crate::grammar::Crates;
use crate::rust::term;

/// Run a program through codegen and MiniRust execution, returning stdout.
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

/// A shared writer for capturing stdout/stderr.
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

#[test]
fn milestone_1_hello_world() {
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
fn milestone_2_booleans_and_multiple_prints() {
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
fn milestone_3_let_bindings() {
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
fn milestone_4_assignment() {
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
fn milestone_5_function_calls() {
    let output = run_program(
        "[crate test {
            fn add_one(x: i32) -> i32 {
                return x;
            }
            fn main() -> () {
                let y: i32 = add_one::<>(1 _ i32);
                print y;
            }
        }]",
    );
    assert_eq!(output, "1\n");
}

#[test]
fn milestone_6_generic_function_calls() {
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
fn milestone_7_if_statements() {
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
fn milestone_8_loops_break_continue() {
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
fn milestone_9_nested_blocks_and_exists() {
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
fn milestone_10_structs() {
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
