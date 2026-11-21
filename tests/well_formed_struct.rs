use a_mir_formality::{crates, FormalityTest};

#[test]
fn struct_with_lifetime() {
    FormalityTest::new(crates![crate test {
        struct Foo<'a> {
            y: &'a u32,
        }

        fn main() -> () {
            exists<'y> {
                let a: u32 = 22_u32;
                let f: Foo<'y> = Foo::<'y> { y: &'y a };
            }
        }
    }])
    .ok()
}

#[test]
fn unused_type_param_is_rejected() {
    FormalityTest::new(crates![crate test {
        struct S<T> {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "check adt" at (adts.rs) failed because
          parameter `!ty_1` is never used in a field of the type"#]]);
}

#[test]
fn unused_lifetime_param_is_rejected() {
    FormalityTest::new(crates![crate test {
        struct S<'a> {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "check adt" at (adts.rs) failed because
          parameter `!lt_1` is never used in a field of the type"#]]);
}

#[test]
fn type_param_used_only_in_where_clause_is_rejected() {
    FormalityTest::new(crates![crate test {
        trait Tr {}
        struct S<T> where T: Tr {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "check adt" at (adts.rs) failed because
          parameter `!ty_1` is never used in a field of the type"#]]);
}

#[test]
fn used_type_param_is_accepted() {
    FormalityTest::new(crates![crate test {
        struct S<T> { value: T }
    }])
    .skip_execute()
    .ok()
}

#[test]
fn used_lifetime_param_is_accepted() {
    FormalityTest::new(crates![crate test {
        struct S<'a> { value: &'a u32 }
    }])
    .skip_execute()
    .ok()
}
