use a_mir_formality::{crates, FormalityTest};

#[formality_core::test]
fn nested_borrow() {
    FormalityTest::new(crates![crate Foo {
        fn foo<'a, 'b>(x: &'a &'b u32) -> &'a &'b u32
        {
            exists<> {
                return x;
            }
        }
    }])
    .skip_execute()
    .ok()
}

#[formality_core::test]
fn extra_nested_borrow() {
    FormalityTest::new(crates![crate Foo {
        fn foo<'a, 'b, 'c>(x: &'a &'b &'c u32) -> &'a &'b &'c u32
        {
            exists<> {
                return x;
            }
        }
    }])
    .skip_execute()
    .ok()
}

#[formality_core::test]
fn indirect_nested_borrow() {
    FormalityTest::new(crates![crate Foo {
        fn foo<'a, 'b, 'c, 'd>(x: &'a &'b u32) -> &'c &'d u32
        where
           'a : 'c,
           'b : 'd

        {
            exists<> {
                return x;
            }
        }
    }])
    .skip_execute()
    .ok()
}

#[test]
fn lifetime() {
    FormalityTest::new(crates![crate Foo {
        // fn one_lt_arg<'a, T>(_: &'a T) -> () {}
        fn one_lt_arg<'a, T>(v0: &'a T) -> ()
        { trusted }
    }])
    .skip_execute()
    .ok()
}
