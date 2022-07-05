#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust
   [(Rust/FnDecl (term (fn foo[(lifetime a) (type T)]((& a T)) -> (& a T)
                           where []
                           trusted-fn-body)))]
                           
   (;; Test that we can write a function
    traced '()
           (test-equal
            #t
            (term (rust:is-core-crate-ok [Rust/FnDecl]))))
   )
  )
