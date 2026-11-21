use a_mir_formality::{crates, FormalityTest};

#[test]
fn i128_is_copy() {
    FormalityTest::new(crates![crate test {
        fn require_copy<T>() -> () where T: Copy { trusted }
        fn main() -> () { require_copy::<i128>(); }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn u128_is_copy() {
    FormalityTest::new(crates![crate test {
        fn require_copy<T>() -> () where T: Copy { trusted }
        fn main() -> () { require_copy::<u128>(); }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn trait_with_valid_fn() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                fn a() -> ();
            }
        }
    ])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn trait_with_valid_associated_type() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                type Assoc : [];
            }
        }
    ])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
#[ignore = "ensures bounds WF check not yet implemented, see FIXME(#228)"]
fn trait_with_ill_formed_where_clause() {
    FormalityTest::new(crates![
        crate core {
            trait A<T> where T: B {}
            trait B {}
            trait C {
                type Assoc : [ A<u32> ];
            }
        }
    ])
    .rustc_err(expect_test::expect![[r#""#]])
    .err(expect_test::expect![[r#"
            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn associated_type_static_outlives_bound() {
    FormalityTest::new(crates![crate Foo {
        trait A { type Item: ['static]; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn associated_type_outer_lifetime_outlives_bound() {
    FormalityTest::new(crates![crate Foo {
        trait A<'a> { type Item: ['a]; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn associated_type_trait_and_outlives_bounds() {
    FormalityTest::new(crates![crate Foo {
        trait Bar {}
        trait A<'a> { type Item: [Bar, 'a]; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_with_unconstrained_type_parameter() {
    FormalityTest::new(crates![crate Foo {
        trait Example {}
        impl<T> Example for u32 {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0207]: the type parameter `T00` is not constrained by the impl trait, self type, or predicates
         --> lib.rs
          |
        3 | impl<T00> Example for u32 {}
          |      ^^^ unconstrained type parameter

        For more information about this error, try `rustc --explain E0207`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !ty_1"#]]);
}

#[test]
fn impl_parameter_type_in_self_type() {
    FormalityTest::new(crates![crate Foo {
        trait Tr {}
        impl<T> Tr for T {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_type_in_trait_argument() {
    FormalityTest::new(crates![crate Foo {
        trait Tr<T> {}
        impl<T> Tr<T> for () {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_type_in_nested_self_type() {
    FormalityTest::new(crates![crate Foo {
        trait Tr {}
        struct Wrapper<T> { value: T }
        impl<T> Tr for Wrapper<T> {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_const_in_trait_argument() {
    FormalityTest::new(crates![crate Foo {
        trait Tr<const C> where type_of_const C is u32 {}
        impl<const C> Tr<C> for () where type_of_const C is u32 {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_const_in_self_type() {
    FormalityTest::new(crates![crate Foo {
        trait Tr {}
        struct Token<const C> where type_of_const C is u32 { value: u32 }
        impl<const C> Tr for Token<C> where type_of_const C is u32 {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_unused_const() {
    FormalityTest::new(crates![crate Foo {
        trait Tr {}
        impl<const C> Tr for () where type_of_const C is u32 {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0207]: the const parameter `N00` is not constrained by the impl trait, self type, or predicates
         --> lib.rs
          |
        3 | impl<const N00: u32> Tr for () {}
          |      ^^^^^^^^^^^^^^ unconstrained const parameter
          |
          = note: expressions using a const parameter must map each value to a distinct output value
          = note: proving the result of expressions other than the parameter are unique is not supported

        For more information about this error, try `rustc --explain E0207`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !const_1"#]]);
}

#[test]
fn impl_parameter_unused_lifetime() {
    FormalityTest::new(crates![crate Foo {
        trait Tr {}
        impl<'a> Tr for () {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_lifetime_only_in_associated_type() {
    FormalityTest::new(crates![crate Foo {
        trait Tr { type Item: []; }
        impl<'a> Tr for () { type Item = &'a u32; }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0207]: the lifetime parameter `'a00` is not constrained by the impl trait, self type, or predicates
         --> lib.rs
          |
        5 | impl<'a00> Tr for () {
          |      ^^^^ unconstrained lifetime parameter

        For more information about this error, try `rustc --explain E0207`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !lt_1"#]]);
}

#[test]
fn impl_parameter_lifetime_in_self_type() {
    FormalityTest::new(crates![crate Foo {
        trait Tr { type Item: []; }
        impl<'a> Tr for &'a u32 { type Item = &'a u32; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_projection_input_does_not_constrain() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        trait Tr {}
        impl<T> Tr for <T as Source>::Item where T: Source {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0207]: the type parameter `T00` is not constrained by the impl trait, self type, or predicates
         --> lib.rs
          |
        7 | impl<T00> Tr for <T00 as Source>::Item where T00: Source {}
          |      ^^^ unconstrained type parameter

        For more information about this error, try `rustc --explain E0207`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !ty_1"#]]);
}

#[test]
fn impl_parameter_nested_projection_input_does_not_constrain() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        trait Tr {}
        struct Wrapper<T> { value: T }
        impl<T> Tr for Wrapper<<T as Source>::Item> where T: Source {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0207]: the type parameter `T00` is not constrained by the impl trait, self type, or predicates
          --> lib.rs
           |
        11 | impl<T00> Tr for Wrapper<<T00 as Source>::Item> where T00: Source {}
           |      ^^^ unconstrained type parameter

        For more information about this error, try `rustc --explain E0207`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !ty_1"#]]);
}

#[test]
fn impl_parameter_negative_impl_unused_type() {
    FormalityTest::new(crates![crate Foo {
        #![feature(negative_impls)]
        trait Tr {}
        impl<T> !Tr for () {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0207]: the type parameter `T00` is not constrained by the impl trait, self type, or predicates
         --> lib.rs
          |
        5 | impl<T00> !Tr for () {}
          |      ^^^ unconstrained type parameter

        For more information about this error, try `rustc --explain E0207`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !ty_1"#]]);
}

#[test]
fn impl_parameter_negative_impl_type_in_self_type() {
    FormalityTest::new(crates![crate Foo {
        #![feature(negative_impls)]
        trait Tr {}
        impl<T> !Tr for T {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn impl_parameter_trait_bound_does_not_constrain() {
    FormalityTest::new(crates![crate Foo {
        trait Bound<T> {}
        trait Tr {}
        impl<T, U> Tr for U where U: Bound<T> {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0207]: the type parameter `T00` is not constrained by the impl trait, self type, or predicates
         --> lib.rs
          |
        5 | impl<T00, T01> Tr for T01 where T01: Bound<T00> {}
          |      ^^^ unconstrained type parameter

        For more information about this error, try `rustc --explain E0207`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {!ty_2}
            parameter = !ty_1"#]]);
}

// AliasEq where-clauses cannot yet be translated to Rust by the test harness.
#[test]
fn impl_parameter_alias_equality_constrains_output() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        trait Tr {}
        impl<U, T> Tr for U
        where U: Source, <U as Source>::Item => T
        {}
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_parameter_alias_equality_reverse_chain() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        trait Tr {}
        impl<U, T, V> Tr for U
        where
            U: Source,
            T: Source,
            <T as Source>::Item => V,
            <U as Source>::Item => T
        {}
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_parameter_alias_equality_needs_constrained_input() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        trait Tr {}
        impl<U, T> Tr for ()
        where U: Source, <U as Source>::Item => T
        {}
    }])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !ty_1"#]]);
}

#[test]
fn impl_parameter_alias_equality_cycle() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        trait Tr {}
        impl<T, U> Tr for ()
        where
            T: Source,
            U: Source,
            <T as Source>::Item => U,
            <U as Source>::Item => T
        {}
    }])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !ty_1"#]]);
}

#[test]
fn impl_parameter_current_impl_projection_does_not_constrain() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        impl<T> Source for u32 where <u32 as Source>::Item => T {
            type Item = T;
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {}
            parameter = !ty_1"#]]);
}

#[test]
fn impl_parameter_nested_projection_output_does_not_constrain() {
    FormalityTest::new(crates![crate Foo {
        trait Source { type Item: []; }
        trait Tr {}
        impl<U, T> Tr for U
        where
            U: Source,
            T: Source,
            <U as Source>::Item => <T as Source>::Item
        {}
    }])
    .err(expect_test::expect![[r#"
        the rule "impl parameters are constrained" at (constrained_parameters.rs) failed because
          condition evaluated to false: `constrained.contains(parameter)`
            constrained = {!ty_1}
            parameter = !ty_2"#]]);
}

#[test]
fn impl_parameter_higher_ranked_equality() {
    FormalityTest::new(crates![crate Foo {
        trait Source<'a> { type Item: []; }
        trait Tr {}
        impl<U, T> Tr for U
        where
            for<'a> U: Source<'a>,
            for<'a> <U as Source<'a>>::Item => T
        {}
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_parameter_associated_type_local_lifetime() {
    FormalityTest::new(crates![crate Foo {
        trait Tr { type Item<'b>: []; }
        impl<'a> Tr for () { type Item<'b> = &'b u32; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}
