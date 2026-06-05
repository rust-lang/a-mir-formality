#![allow(non_snake_case)] // we embed type names into the names for our test functions
use a_mir_formality::{crates, FormalityTest};




#[test]
fn duplicate_feature_gate() {
    FormalityTest::new(crates![crate Foo 
        { 
            #![feature(generic_atomic)]
            #![feature(generic_atomic)]


    }])
    .err(expect_test::expect![[r#"test"#]])
}