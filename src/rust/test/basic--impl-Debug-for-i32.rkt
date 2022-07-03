#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  ;; Program:
  ;;
  ;; trait Debug { }
  ;; impl Debug for i32 { }
  (redex-let*
   formality-rust

   ((Rust/TraitDecl (term (trait Debug[] where [] {})))
    (Rust/TraitImplDecl (term (impl[] Debug[] for i32 where [] {})))
    (Rust/CrateDecl (term (crate TheCrate { Rust/TraitDecl Rust/TraitImplDecl })))
    (Rust/Program (term ([Rust/CrateDecl TheCrate])))
    )

   (traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program Rust/Program (i32 : Debug[])))
            #t))
   )
  )