use a_mir_formality::{crates, FormalityTest};

#[test]
fn single_feature_gate() {
    FormalityTest::new(crates![crate Foo
        {
            #![feature(generic_atomic)]

    }])
    .ok()
}

#[test]
fn two_feature_gates() {
    FormalityTest::new(crates![crate Foo
        {
            #![feature(generic_atomic)]
            #![feature(polonius_alpha)]

    }])
    .ok()
}

#[test]
fn duplicate_feature_gate() {
    FormalityTest::new(crates![crate Foo
        {
            #![feature(generic_atomic)]
            #![feature(generic_atomic)]

    }])
    .err(expect_test::expect![[r#"
        the rule "check feature gate" at (mod.rs) failed because
          condition evaluated to false: `crate_items.iter().filter(|i|
          matches!(i, CrateItem::FeatureGate(fg) if fg == feature_gate)).count() <= 1`"#]])
}
