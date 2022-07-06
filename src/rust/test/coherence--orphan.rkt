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
    (Rust/TraitImplDecl_AforA (term (impl[] TraitA[] for (StructA < >) where [] {})))
    ]

   (; both trait/struct in same crate
    traced '()
           (test-equal
            #t
            (term (rust:is-core-crate-ok [Rust/TraitDecl_A
                                          Rust/StructDecl_A
                                          Rust/TraitImplDecl_AforA
                                          ]))))

   (; trait/struct in parent crate, impl in child -- error
    traced '()
           (test-equal
            #f
            (term (rust:is-program-ok ([(crate A { Rust/TraitDecl_A Rust/StructDecl_A })
                                        (crate B { Rust/TraitImplDecl_AforA })
                                        ]
                                       B)))))

   (; trait in parent crate, struct/impl in child -- ok
    traced '()
           (test-equal
            #t
            (term (rust:is-program-ok ([(crate A { Rust/TraitDecl_A })
                                        (crate B { Rust/StructDecl_A Rust/TraitImplDecl_AforA })
                                        ]
                                       B)))))

   (; struct in parent crate, trait/impl in child -- ok
    traced '()
           (test-equal
            #t
            (term (rust:is-program-ok ([(crate A { Rust/StructDecl_A })
                                        (crate B { Rust/TraitDecl_A Rust/TraitImplDecl_AforA })
                                        ]
                                       B)))))

   )
  )