#![allow(non_snake_case)]

use a_mir_formality::{crates, FormalityTest};

#[test]
fn ok() {
    FormalityTest::new(crates![crate Foo {
        // fn simple_fn() {}
        fn simple_fn() -> () { trusted }

        // fn one_arg<T>(_: T) {}
        fn one_arg<T>(v0: T) -> () { trusted }

        // fn one_ret<T>(_: T) {}
        fn one_ret<T>() -> T { trusted }

        // fn arg_ret<T, U>(_: T) -> U {}
        fn arg_ret<T, U>(v0: T) -> U { trusted }

        // fn multi_arg_ret<T, Y, U, I>(_: T, _: Y) -> (U, I) {}
        fn multi_arg_ret<T, Y, U, I>(v0: T, v1: Y) -> (U, I) { trusted }
    }])
    .skip_execute()
    .ok()
}

#[test]
fn lifetime() {
    FormalityTest::new(crates![crate Foo {
        // fn one_lt_arg<'a, T>(_: &'a T) -> () {}
        fn one_lt_arg<'a, T>(v0: &'a T) -> ()
        where
            T: 'a, // FIXME(#202): Implied bounds should not have to be explicit
        { trusted }
    }])
    .skip_execute()
    .ok()
}

#[test]
fn omitted_return_type() {
    FormalityTest::new(crates![
        crate Foo {
            fn simple_fn() { trusted }
            fn one_arg<T>(v0: T) { trusted }
            fn with_where<'a, T>(v0: &'a T)
            where
                T: 'a,
            { trusted }
        }
    ])
    .skip_execute()
    .ok()
}
