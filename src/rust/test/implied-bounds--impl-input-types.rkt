#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test
  ; In this test, the associated type requires T: 'a, and...
  ;
  ; * in variant A, this is implied by the impl self type `Foo<'a, T>`
  ;   because of the where-clause on the struct
  ; * in variant B, it is not
  (redex-let*
   formality-rust
   [([Rust/CrateItemDecl ...]
     (term [(trait SomeTrait[]
                   where []
                   { (type SomeType[] : [] where [])
                     })

            (impl[(lifetime a) (type T)] SomeTrait[] for (Foo < a T >)
                 where []
                 { (type SomeType[] = (& a T) where [])
                   }
                 )
            ]))]

   ; Variant A: struct includes where clause, program ok
   (test-term-true
    (rust:is-core-crate-ok
     [Rust/CrateItemDecl ...
      (struct Foo[(lifetime a) (type T)]
        where [(T : a)]
        {})]
     ))

   ; Variant B: struct does NOT include where clause, program NOT ok
   (test-term-false
    (rust:is-core-crate-ok
     [Rust/CrateItemDecl ...
      (struct Foo[(lifetime a) (type T)]
        where []
        {})]
     ))
   )
  )