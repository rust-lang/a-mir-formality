#![cfg(FIXME)]
#![allow(non_snake_case)]

#[test]
#[ignore]
fn magic_copy() {
    const PROGRAM: &str = "[
        crate core {
            struct Foo {}
            trait Copy {}
            trait Magic where Self: Copy {}

            impl<ty T> Magic for T where T: Magic {}
        }
    ]";

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));

    expect_test::expect![[r#"
        Ok(
            no,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_where_clause(
        PROGRAM,
        "Foo: Magic",
    ));
}

#[test]
#[ignore]
fn magic_copy_impl_for_all_copy() {
    const PROGRAM: &str = "[
        crate core {
            struct Foo {}
            struct Vec<ty T> {}

            trait Copy {}
            trait Magic where Self: Copy {}

            impl<ty T> Magic for T where T: Copy {}
        }
    ]";

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));

    // no copy impl, so this doesn't hold
    expect_test::expect![[r#"
        Ok(
            no,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_goal(
        PROGRAM,
        "is_implemented(Magic(Foo))",
    ));

    // no copy impl, so this doesn't hold
    expect_test::expect![[r#"
        Ok(
            yes,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_goal(
        PROGRAM,
        "implies([is_implemented(Copy(Foo))], is_implemented(Magic(Foo)))",
    ));
}

#[test]
#[ignore]
fn magic_vec_t() {
    const PROGRAM: &str = "[
        crate core {
            struct Foo {}
            struct Vec<ty T> {}

            trait Copy {}
            trait Magic where Self: Copy {}

            impl<ty T> Magic for Vec<T> where T: Magic {
                // FIXME: We need to test that this impl can prove T: Copy,
                // but how to do it?
            }
            impl<ty T> Copy for Vec<T> where T: Magic {}
        }
    ]";

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}

// (module+ test
//     (; Magic trait, implemented in terms of itself, that extends Copy
//      redex-let*
//      formality-rust

//      [(Rust/Program
//        (term ([(crate TheCrate { (struct Foo[] { (counter : i32) })
//                                  (struct Bar[] { (counter : i32) })
//                                  (trait Magic[] where [(Self : Copy[])] {})
//                                  (trait Copy[] {})
//                                  (impl[(type T)] Magic[] for T where [(T : Magic[])] {})
//                                  (impl[] Copy[] for (Bar < >) {})
//                                  })]
//               TheCrate)))
//       ]

//      (; All decls in crate are considered 'ok'. In particular, the impl is considered 'ok',
//       ; since its where clauses allow it to locally prove that `Self: Copy`.
//       traced '()
//              (test-equal
//               #t
//               (term (rust:is-program-ok Rust/Program))))

//      (; ...but when we try to use it, we cannot prove that `Foo: Magic`
//       ; because `Foo: Copy` does not hold...
//       traced '()
//              (test-equal
//               #f
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Magic[])))))

//      (; ...also cannot prove that `Foo Copy`, of course.
//       traced '()
//              (test-equal
//               #f
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Copy[])))))

//      (; Can prove that `Bar: Magic` -- it has a copy impl
//       traced '()
//              (test-equal
//               #t
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Magic[])))))

//      (; And can prove that `Bar: Copy` -- it has a copy impl
//       traced '()
//              (test-equal
//               #t
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Copy[])))))
//      )

//     (; Mutual recursion between Magic and Copy, with Magic implemented in terms of itself,
//      ; but no impl of Copy
//      redex-let*
//      formality-rust

//      [(Rust/Program (term ([(crate C { (struct Foo[] { (counter : i32) })
//                                        (struct Bar[] { (counter : i32) })
//                                        (trait Magic[] where [(Self : Copy[])] {})
//                                        (trait Copy[] where [(Self : Magic[])] {})
//                                        (impl[(type T)] Magic[] for T where [(T : Magic[])] {})
//                                        (impl[] Copy[] for (Bar < >) {})
//                                        })] C)))]

//      (; All decls in crate are considered 'ok'.
//       traced '()
//              (test-equal
//               #t
//               (term (rust:is-program-ok Rust/Program))))

//      (; Cannot prove that `Foo Magic`
//       traced '()
//              (test-equal
//               #f
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Magic[])))))

//      (; And cannot prove that `Foo Copy`
//       traced '()
//              (test-equal
//               #f
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Copy[])))))

//      (; Can prove that `Bar: Magic` -- it has a copy impl
//       traced '()
//              (test-equal
//               #t
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Magic[])))))

//      (; And can prove that `Bar: Copy` -- it has a copy impl
//       traced '()
//              (test-equal
//               #t
//               (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Copy[])))))
//      )

//     )
// #lang racket
// (require redex/reduction-semantics
//          "../../util.rkt"
//          "../grammar.rkt"
//          "../prove.rkt"
//          )

// (module+ test
//   (redex-let*
//    formality-rust

//    [([Rust/CrateItemDecl_core ...] (term [(struct Foo[] {})
//                                           (trait Copy[] {})
//                                           (trait Partial[] where (Self : Copy()) {})
//                                           (trait Complete[] where (Self : Partial()) {})
//                                           (impl[(type T)] Partial[] for T where (T : Complete()) {})]))
//     (Rust/Program_A (term ([(crate A { Rust/CrateItemDecl_core ...
//                                        (impl[(type T)] Complete[] for T {})
//                                        })]
//                            A)))
//     (Rust/Program_B (term ([(crate B { Rust/CrateItemDecl_core ...
//                                        (impl[(type T)] Complete[] for T where [(T : Partial[])] {})
//                                        })]
//                            B)))
//     ]

//    (; The program A is not well-formed:
//     ;
//     ; the `impl<T> Complete for T` cannot prove that `T: Complete` because it cannot
//     ; prove that `T: Copy`.
//     traced '()
//            (test-equal #f
//                        (term (rust:is-program-ok Rust/Program_A))))

//    (; The program B, however, IS well-formed.
//     traced '()
//            (test-equal #t
//                        (term (rust:is-program-ok Rust/Program_B))))

//    (; But `Foo: Partial` does not hold in B.
//     traced '()
//            (test-equal #f
//                        (term (rust:can-prove-where-clause-in-program
//                               Rust/Program_B
//                               ((Foo < >) : Partial[])))))

//    (; But `Foo: Partial` implies `Foo: Copy`.
//     traced '()
//            (test-equal #t
//                        (term (rust:can-prove-where-clause-in-program
//                               Rust/Program_B
//                               (∀ []
//                                  where [((Foo < >) : Partial[])]
//                                  ((Foo < >) : Copy[]))))))

//    )
//   )
