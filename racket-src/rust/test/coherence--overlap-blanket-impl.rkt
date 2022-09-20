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
    (Rust/TraitDecl_B (term (trait TraitB[] where [] {})))
    (Rust/StructDecl_A (term (struct StructA[] where [] {})))
    ]

   (; one impl for `StructA` and one for for any type T where `T: TraitB`
    ;
    ; should be ok because `StructA` does not implement `TraitB`
    traced '()
           (test-equal
            (term (rust:is-core-crate-ok [Rust/TraitDecl_A
                                          Rust/TraitDecl_B
                                          Rust/StructDecl_A
                                          (impl[] TraitA[] for (StructA < >) where [] {})
                                          (impl[(type T)] TraitA[] for T where [(T : TraitB[])] {})
                                          ]))
            #t
            ))

   (redex-let*
    formality-rust
    [
     (Rust/CrateDecl_one (term (crate one { Rust/TraitDecl_B
                                            Rust/StructDecl_A
                                            })))
     (Rust/CrateDecl_two (term (crate two { Rust/TraitDecl_A
                                            (impl[] TraitA[] for (StructA < >) where [] {})
                                            (impl[(type T)] TraitA[] for T where [(T : TraitB[])] {})
                                            })))
     (Rust/Program (term ([Rust/CrateDecl_one Rust/CrateDecl_two] two)))
     ]
    (; one impl for `StructA` and one for for any type T where `T: TraitB`
     ;
     ; should be ok because `StructA` does not implement `TraitB`
     traced '()
            (test-equal
             (term (rust:is-program-ok Rust/Program))
             #t ; FIXME: This should be #f, but we don't understand cross-crate rules
             ))))
  )