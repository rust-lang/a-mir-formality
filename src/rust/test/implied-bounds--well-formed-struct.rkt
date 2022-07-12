#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test

  (redex-let*
   formality-rust

   ((; struct Foo<'a, T> where T: 'a { }
     Rust/AdtDecl_Foo (term (struct Foo[(lifetime a) (type T)]
                              where [(T : a)] {})))
    (Rust/CrateDecl_C (term (crate C { Rust/AdtDecl_Foo })))
    )

   (traced '()
           (test-equal (term (rust:can-prove-goal-in-program
                              ([Rust/CrateDecl_C] C)
                              (∀
                               [(type A) (lifetime x)]
                               (implies
                                ((well-formed (type (user-ty (Foo < x A >)))))
                                (A -outlives- x)))
                              ))
                       #t)) ; FIXME
   )
  )

