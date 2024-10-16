#![allow(non_snake_case)]

#[test]
fn ok() {
    crate::assert_ok!(
        // Test functions, function's arguments, and function's returns
        //@check-pass
        [
            crate Foo {
                // fn simple_fn() {}
                fn simple_fn() -> () { trusted }

                // fn one_arg<T>(_: T) {}
                fn one_arg<ty T>(T) -> () { trusted }

                // fn one_ret<T>(_: T) {}
                fn one_ret<ty T>() -> T { trusted }

                // fn arg_ret<T, U>(_: T) -> U {}
                fn arg_ret<ty T, ty U>(T) -> U { trusted }

                // fn multi_arg_ret<T, Y, U, I>(_: T, _: Y) -> (U, I) {}
                fn multi_arg_ret<ty T, ty Y, ty U, ty I>(T, Y) -> (U, I) { trusted }
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn lifetime() {
    crate::assert_ok!(
        // Test lifetimes on function
        [
            crate Foo {
                // fn one_lt_arg<'a, T>(_: &'a T) -> () {}
                fn one_lt_arg<lt a, ty T>(&a T) -> () { trusted }
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn well_formed_function_ptrs() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo<ty T>() -> T { trusted }
                fn bar(fn foo<u32>, fn(u32) -> (), fn(bool)) -> () { trusted }
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn not_well_formed_fn_def() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo {}
                fn foo<ty T>() -> T where T: Foo { trusted }
                fn bar(fn foo<u32>) -> () { trusted }
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            judgment `prove { goal: {@ wf(fn foo<u32>)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [], [], [], [], [], [fn foo <ty> ([]) -> ^ty0_0 where {Foo(^ty0_0)}, fn bar ([fn foo<u32>]) -> ()], {Foo}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goal: {@ wf(fn foo<u32>)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: @ wf(fn foo<u32>), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "parameter well formed" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wf { goal: fn foo<u32>, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "fn-defs" failed at step #3 (src/file.rs:LL:CC) because
                            judgment `prove_after { constraints: Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }, goal: {Foo(u32)}, assumptions: {} }` failed at the following rule(s):
                              the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `prove { goal: {Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [], [], [], [], [], [fn foo <ty> ([]) -> ^ty0_0 where {Foo(^ty0_0)}, fn bar ([fn foo<u32>]) -> ()], {Foo}, {}) }` failed at the following rule(s):
                                  failed at (src/file.rs:LL:CC) because
                                    judgment `prove_wc_list { goal: {Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                        judgment `prove_wc { goal: Foo(u32), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                          the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                            expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn not_well_formed_function_ptr() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo {}
                struct baz<ty T> where T: Foo {}
                fn foo<ty T>(fn(baz<T>)) -> () { trusted }
            }
        ]

        [ r#"the rule "fn-ptr" failed"#, ]

        expect_test::expect![[r#"
            judgment `prove { goal: {@ wf(fn(baz<!ty_0>) -> ())}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [], [], [], [], [adt baz <ty> where {Foo(^ty0_0)}], [fn foo <ty> ([fn(baz<^ty0_0>) -> ()]) -> ()], {Foo}, {baz}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goal: {@ wf(fn(baz<!ty_0>) -> ())}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: @ wf(fn(baz<!ty_0>) -> ()), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                      the rule "parameter well formed" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wf { goal: fn(baz<!ty_0>) -> (), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                          the rule "fn-ptr" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_wf { goal: baz<!ty_0>, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                              the rule "ADT" failed at step #3 (src/file.rs:LL:CC) because
                                judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness }, known_true: true, substitution: {} }, goal: {Foo(!ty_0)}, assumptions: {} }` failed at the following rule(s):
                                  the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment `prove { goal: {Foo(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [], [], [], [], [adt baz <ty> where {Foo(^ty0_0)}], [fn foo <ty> ([fn(baz<^ty0_0>) -> ()]) -> ()], {Foo}, {baz}) }` failed at the following rule(s):
                                      failed at (src/file.rs:LL:CC) because
                                        judgment `prove_wc_list { goal: {Foo(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                          the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                            judgment `prove_wc { goal: Foo(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                              the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                                expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}
