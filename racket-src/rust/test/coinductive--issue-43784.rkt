#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [([Rust/CrateItemDecl_core ...] (term [(struct Foo[] where [] {})
                                          (trait Copy[] where [] {})
                                          (trait Partial[] where [(Self : Copy())] {})
                                          (trait Complete[] where [(Self : Partial())] {})
                                          (impl[(type T)] Partial[] for T where [(T : Complete())] {})]))
    (Rust/Program_A (term ([(crate A { Rust/CrateItemDecl_core ...
                                       (impl[(type T)] Complete[] for T where [] {})
                                       })]
                           A)))
    (Rust/Program_B (term ([(crate B { Rust/CrateItemDecl_core ...
                                       (impl[(type T)] Complete[] for T where [(T : Partial[])] {})
                                       })]
                           B)))
    ]

   (; The program A is not well-formed:
    ;
    ; the `impl<T> Complete for T` cannot prove that `T: Complete` because it cannot
    ; prove that `T: Copy`.
    traced '()
           (test-equal #f
                       (term (rust:is-program-ok Rust/Program_A))))

   (; The program B, however, IS well-formed.
    traced '()
           (test-equal #t
                       (term (rust:is-program-ok Rust/Program_B))))

   (; But `Foo: Partial` does not hold in B.
    traced '()
           (test-equal #f
                       (term (rust:can-prove-where-clause-in-program
                              Rust/Program_B
                              ((Foo < >) : Partial[])))))

   (; But `Foo: Partial` implies `Foo: Copy`.
    traced '()
           (test-equal #t
                       (term (rust:can-prove-where-clause-in-program
                              Rust/Program_B
                              (∀ []
                                 where [((Foo < >) : Partial[])]
                                 ((Foo < >) : Copy[]))))))

   )
  )
