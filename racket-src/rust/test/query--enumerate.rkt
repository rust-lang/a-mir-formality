#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../logic/alpha-eq-util.rkt"
         "../query.rkt"
         )

(module+ test

  (traced '()
          (test-equal
           (term (rust:query
                  ([(crate C { (struct String[] where [] { })
                               (trait Display[] where [] { })
                               (impl[] Display[] for (String < >) where [] { })
                               })]
                   C)
                  (?∃ [(type T)]
                      (T : Display[])))
                 )
           (term [ambiguous])      ; no additional constraints
           )
          )

  )