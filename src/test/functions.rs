#![allow(non_snake_case)]

#[test]
fn ok() {
    crate::assert_ok!(


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
    )
}

#[test]
fn lifetime() {
    crate::assert_ok!(

        [
            crate Foo {
                // fn one_lt_arg<'a, T>(_: &'a T) -> () {}
                fn one_lt_arg<lt a, ty T>(&a T) -> ()
                where
                    T: a, // FIXME(#202): Implied bounds should not have to be explicit
                { trusted }
            }
        ]
    )
}
