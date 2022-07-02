#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../prove.rkt"
         "../grammar.rkt"
         )

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test

  (redex-let*
   formality-rust

   ((; struct Foo<'a, T> where T: 'a { r : &'a T }
     Rust/AdtDecl_Foo (term (struct Foo[(lifetime a) (type T)]
                              where [((type T) : (lifetime a))]
                              { (r : (& a T)) })))
    (Rust/CrateDecl_C (term (crate C {Rust/AdtDecl_Foo})))
    )

   (traced '() (test-equal (term (rust:is-program-ok ([Rust/CrateDecl_C] C)))
                           #t))
   )

  (redex-let*
   formality-rust

   ((; struct Foo<'a, T> { r: &'a T }
     ;
     ; ERROR, because we don't model outlives inference
     Rust/AdtDecl_Foo (term (struct Foo[(lifetime a) (type T)]
                              where []
                              { (r : (& a T)) })))
    (Rust/CrateDecl_C (term (crate C { Rust/AdtDecl_Foo })))
    )

   (traced '() (test-equal (term (rust:is-program-ok ([Rust/CrateDecl_C] C)))
                           #f))
   )
  )

