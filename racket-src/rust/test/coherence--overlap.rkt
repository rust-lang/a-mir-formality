#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(Rust/TraitDecl_A (term (trait TraitA[] where [] {})))
    (Rust/StructDecl_A (term (struct StructA[] where [] {})))
    ]

   (; one impl for `A` and one for `u32` is ok
    traced '()
           (test-equal
            #t
            (term (rust:is-core-crate-ok [Rust/TraitDecl_A
                                          Rust/StructDecl_A
                                          (impl[] TraitA[] for (StructA < >) where [] {})
                                          (impl[] TraitA[] for u32 where [] {})
                                          ]))))

   (; one impl for `A` and one for for any type T is not ok
    traced '()
           (test-equal
            #f
            (term (rust:is-core-crate-ok [Rust/TraitDecl_A
                                          Rust/StructDecl_A
                                          (impl[] TraitA[] for (StructA < >) where [] {})
                                          (impl[(type T)] TraitA[] for T where [] {})
                                          ]))))

   (; two impls for the same struct is an error
    traced '()
           (test-equal
            #f
            (term (rust:is-core-crate-ok [Rust/TraitDecl_A
                                          Rust/StructDecl_A
                                          (impl[] TraitA[] for (StructA < >) where [] {})
                                          (impl[] TraitA[] for (StructA < >) where [] {})
                                          ]))))
   )
  )
