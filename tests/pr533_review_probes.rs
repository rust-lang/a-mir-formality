use a_mir_formality::{crates, BorrowCheckFailure, FormalityTest};

#[test]
fn projection_bound_through_existential_must_be_checked() {
    let program = crates![crate Foo {
        trait A { type Item: []; }
        fn require<'r, T>() -> &'r u32
        where T: A, <T as A>::Item: 'r { trusted }
        fn caller<'a, T>() -> &'a u32 where T: A {
            exists<'r> { return require::<'r, T>(); }
        }
        fn main() -> () {}
    }];
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(!ok && stderr.contains("E0309"), "{stderr}");
    FormalityTest::new(program).borrowck_err(BorrowCheckFailure::All, expect_test::expect![[""]]);
}

#[test]
fn transitive_projection_assumption_is_sufficient() {
    let program = crates![crate Foo {
        trait A { type Item: []; }
        impl A for u32 { type Item = u32; }
        fn require<'a, T>() -> () where T: A, <T as A>::Item: 'a {}
        fn caller<'a, 'b, T>() -> ()
        where T: A, <T as A>::Item: 'b, 'b: 'a {
            require::<'a, T>();
        }
        fn main() -> () { caller::<'static, 'static, u32>(); println!(7_u32); }
    }];
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(ok, "{stderr}");
    FormalityTest::new(program)
        .rustc_ok()
        .expect_output("7\n")
        .borrowck_ok();
}

#[test]
fn trait_static_bound_is_sufficient() {
    let program = crates![crate Foo {
        trait A { type Item: ['static]; }
        impl A for u32 { type Item = u32; }
        fn require<'a, T>() -> () where T: A, <T as A>::Item: 'a {}
        fn caller<'a, T>() -> () where T: A, 'a: 'a { require::<'a, T>(); }
        fn main() -> () { caller::<'static, u32>(); println!(9_u32); }
    }];
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(ok, "{stderr}");
    FormalityTest::new(program)
        .rustc_ok()
        .expect_output("9\n")
        .borrowck_ok();
}
