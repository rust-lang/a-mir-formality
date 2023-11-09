#![cfg(FIXME)]
#![allow(non_snake_case)]

#[test]
#[ignore]
fn absolutely_same() {
    const PROGRAM: &str = "[
        crate core {
            trait Debug {}
            trait Display {}

            trait Get {
                fn get<ty T, lt l>(&mut l T) -> () where [T: Debug];
            }

            impl Get for () {
                fn get<ty T, lt l>(&mut l T) -> () where T: Debug {
                    trusted
                }
            }
        }
    ]";

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}

#[test]
#[ignore]
fn different_self_type_mut_vs_sh() {
    const PROGRAM: &str = "[
        crate core {
            trait Debug {}
            trait Display {}

            trait Get {
                fn get<ty T, lt l>(&mut l T) -> () where [T: Debug];
                //                 --------
            }

            impl Get for () {
                fn get<ty T, lt l>(&l T) -> () where T: Debug {
                    //             ----
                    trusted
                }
            }
        }
    ]";

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <> Get((rigid tuple(0))) { fn get <ty, lt> [(rigid &(shared) ^lt0_1 ^ty0_0)] -> (rigid tuple(0)) where [well_formed((rigid &(shared) ^lt0_1 ^ty0_0)), well_formed((rigid tuple(0))), is_implemented(Debug(^ty0_0))] })",
                source: Error {
                    context: "check_fn_in_impl",
                    source: "could not prove `sub((rigid &(mut) !ltU(2)_1 !tyU(2)_0), (rigid &(shared) !ltU(2)_1 !tyU(2)_0))` given `[\n    well_formed((rigid &(shared) !ltU(2)_1 !tyU(2)_0)),\n    well_formed((rigid tuple(0))),\n    is_implemented(Debug(!tyU(2)_0)),\n]`",
                },
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}

#[test]
#[ignore]
fn different_arg_type_u32_vs_i32() {
    const PROGRAM: &str = "[
        crate core {
            trait Debug {}
            trait Display {}

            trait Get {
                fn get<ty T, lt l>(&mut l T, u32) -> () where [T: Debug];
                //                           ---
            }

            impl Get for () {
                fn get<ty T, lt l>(&mut l T, i32) -> () where T: Debug {
                    //                       ---
                    trusted
                }
            }
        }
    ]";

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <> Get((rigid tuple(0))) { fn get <ty, lt> [(rigid &(mut) ^lt0_1 ^ty0_0), (rigid (scalar i32))] -> (rigid tuple(0)) where [well_formed((rigid &(mut) ^lt0_1 ^ty0_0)), well_formed((rigid (scalar i32))), well_formed((rigid tuple(0))), is_implemented(Debug(^ty0_0))] })",
                source: Error {
                    context: "check_fn_in_impl",
                    source: "could not prove `sub((rigid (scalar u32)), (rigid (scalar i32)))` given `[\n    well_formed((rigid &(mut) !ltU(2)_1 !tyU(2)_0)),\n    well_formed((rigid (scalar i32))),\n    well_formed((rigid tuple(0))),\n    is_implemented(Debug(!tyU(2)_0)),\n]`",
                },
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}
