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
    crate::assert_err!(
        // Test lifetimes on function
        [
            crate Foo {
                // fn one_lt_arg<'a, T>(_: &'a T) -> () {}
                fn one_lt_arg<lt a, ty T>(&a T) -> () { trusted }
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            judgment `prove_wc_list { goal: {@ wf(&!lt_0 !ty_1)}, assumptions: {}, env: Env { variables: [!lt_0, !ty_1], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: @ wf(&!lt_0 !ty_1), assumptions: {}, env: Env { variables: [!lt_0, !ty_1], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                  the rule "parameter well formed" failed at step #0 (src/file.rs:LL:CC) because
                    judgment had no applicable rules: `prove_wf { goal: &!lt_0 !ty_1, assumptions: {}, env: Env { variables: [!lt_0, !ty_1], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }`"#]]
    )
}
