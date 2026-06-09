use a_mir_formality::{crates, FormalityTest};

#[test]
fn explicit_void() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> () {
        }
    }])
    .ok()
}

#[test]
fn implicit_void() {
    FormalityTest::new(crates![crate Foo {
        fn foo() {
        }
    }])
    .ok()
}
