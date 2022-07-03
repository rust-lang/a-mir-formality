#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(Rust/Program (term ([(crate C { (trait PartialEq[] where [] {})
                                     (trait Eq[] where [(Self : PartialEq[])] {})
                                     (trait Debug[] where [] {})
                                     })]
                         C)))]

   (traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program
                   Rust/Program
                   (∀ [(type T)]
                      where [(T : PartialEq[])]
                      (T : Eq[]))))
            #f))

   (traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program
                   Rust/Program
                   (∀ [(type T)]
                      where [(T : Eq[])]
                      (T : PartialEq[]))))
            #t))

   (traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program
                   Rust/Program
                   (∀ [(type T)]
                      where [(T : Eq[])]
                      (T : Eq[]))))
            #t))

   (traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program
                   Rust/Program
                   (∀ [(type T)]
                      where [(T : Eq[])]
                      (T : Debug[]))))
            #f))

   )
  )