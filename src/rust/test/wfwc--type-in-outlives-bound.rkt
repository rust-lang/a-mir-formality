#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test
  (traced '()
          (test-term-false ; false because `T: Ord` does not hold
           (rust:is-core-crate-ok
            [(trait Ord[] where [] {})
             (struct IsOrd[(type T)] where [(T : Ord[])] {})
             (fn foo[(lifetime a) (type T)]() -> ()
                 where [((type (IsOrd < T >)) : (lifetime a)) ; for `IsOrd<T>` to be WF, `T: Ord` must hold
                        ]
                 trusted-fn-body)
             ])))

  (traced '()
          (test-term-true
           (rust:is-core-crate-ok
            [(trait Ord[] where [] {})
             (struct IsOrd[(type T)] where [(T : Ord[])] {})
             (fn foo[(lifetime a) (type T)]() -> ()
                 where [((type (IsOrd < T >)) : (lifetime a))
                        (T : Ord[])
                        ]
                 trusted-fn-body)
             ])))

  )