use a_mir_formality::{crates, FormalityTest};

#[test]
fn explicit_return_type() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> () {
        }
    }])
    .ok()
}

#[test]
fn implicit_return_type() {
    FormalityTest::new(crates![crate Foo {
        fn foo() {
        }
    }])
    .ok()
}
