#![allow(non_snake_case)]

#[test]
fn absolutely_same() {
    const PROGRAM: &str = "[
        crate core {
            trait Debug<> where [] {}
            trait Display<> where [] {}

            trait Get<> where [] {
                fn get<ty T, lt l>(&mut l T) -> () where [T: Debug<>];
            }

            impl<> Get<> for () where [] {
                fn get<ty T, lt l>(&mut l T) -> () where [T: Debug<>] {
                    trusted
                }
            }
        }
    ]";

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Get)",
                source: "could not prove `well_formed((rigid &(mut) !ltU(2)_1 !tyU(2)_0))` given `[\n    well_formed((rigid &(mut) !ltU(2)_1 !tyU(2)_0)),\n    well_formed((rigid tuple(0))),\n    is_implemented(Debug(!tyU(2)_0)),\n]`",
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}

#[test]
fn different_self_type_mut_vs_sh() {
    const PROGRAM: &str = "[
        crate core {
            trait Debug<> where [] {}
            trait Display<> where [] {}

            trait Get<> where [] {
                fn get<ty T, lt l>(&mut l T) -> () where [T: Debug<>];
                //                 --------
            }

            impl<> Get<> for () where [] {
                fn get<ty T, lt l>(&l T) -> () where [T: Debug<>] {
                    //             ----
                    trusted
                }
            }
        }
    ]";

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Get)",
                source: "could not prove `well_formed((rigid &(mut) !ltU(2)_1 !tyU(2)_0))` given `[\n    well_formed((rigid &(mut) !ltU(2)_1 !tyU(2)_0)),\n    well_formed((rigid tuple(0))),\n    is_implemented(Debug(!tyU(2)_0)),\n]`",
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}

#[test]
fn different_arg_type_u32_vs_i32() {
    const PROGRAM: &str = "[
        crate core {
            trait Debug<> where [] {}
            trait Display<> where [] {}

            trait Get<> where [] {
                fn get<ty T, lt l>(&mut l T, u32) -> () where [T: Debug<>];
                //                           ---
            }

            impl<> Get<> for () where [] {
                fn get<ty T, lt l>(&mut l T, i32) -> () where [T: Debug<>] {
                    //                       ---
                    trusted
                }
            }
        }
    ]";

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Get)",
                source: "could not prove `well_formed((rigid &(mut) !ltU(2)_1 !tyU(2)_0))` given `[\n    well_formed((rigid &(mut) !ltU(2)_1 !tyU(2)_0)),\n    well_formed((rigid (scalar u32))),\n    well_formed((rigid tuple(0))),\n    is_implemented(Debug(!tyU(2)_0)),\n]`",
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}
