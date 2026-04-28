#[test]
fn deref_custom_pointer() {
    crate::assert_ok!([
        crate test {
            struct Ptr<T> {
                value: *mut T,
            }

            impl<T> Place for Ptr<T> {
                type Target = T;
            }

            fn test<'a, T>(ptr: Ptr<&'a T>) -> ()
            where
                T: 'a,
            {
                // We use `&'a T` here, since `Copy` doesn't exist at this
                // point and references are handled specially.
                //
                // Also note that this actually shouldn't compile, since we
                // have not implemented the `ReadPlace` trait.
                let _: &'a T = *ptr;
            }

            /*
            fn test2<T>(ptr: Ptr<T>) -> () {
                let _: T = *ptr;
            }
            */
        }
    ])
}

// crate test {
//     struct Foo<'a, 'b> {
//         y: &'b u32,
//     }

//     impl<'a, 'b> Place for Foo<'a, 'b>
//     where
//         'a: 'b,
//     {
//         type Target = &'b u32;
//     }

//     fn foo<'x>() -> () {
//         exists<'y> {
//             let a: u32 = 22 _ u32;
//             let f: Foo<'x, 'y> = Foo { y: &'y a };
//             // somehow force `f` to implement `Place`
//             // a = 1 _ u32; // this should error
//         }
//     }
// }

#[test]
fn missing_constraint() {
    crate::assert_err!(
        [
            crate test {
                struct Foo<'a> {
                    y: &'a u32,
                }

                fn foo() -> () {
                    exists<'y> {
                        let a: u32 = 22 _ u32;
                        let f: Foo<'y> = Foo { y: &'y a };
                    }
                }
            }
        ]
        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(Foo<?lt_0>), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove/prove/prove_wf.rs:14:1: no applicable rules for prove_wf { goal: ?lt_0, assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]
    )
}
// expect_test::expect![[r#"
//     crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(Foo<!lt_1, ?lt_0>), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

//     crates/formality-rust/src/prove/prove/prove/prove_wf.rs:14:1: no applicable rules for prove_wf { goal: ?lt_0, assumptions: {@ wf(?lt_0)}, env: Env { variables: [!lt_1, ?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]
