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

#[test]
#[ignore = "to be resolved by (#437)"]
fn implied_bound_for_normalized_ty() {
    FormalityTest::new(crates![
        crate core {
            trait A<'a> {
                type Assoc : [];
            }
            impl<'a, T> A<'a> for T  where T: 'a {
                type Assoc = &'a T;
            }

            fn outlives<'a, T>() -> () where T : 'a {}

            fn test<'a, T>(_: <T as A<'a>>::Assoc) -> () {
                outlives::<'a, T>();
            }
        }
    ])
    .skip_execute()
    .ok();
}