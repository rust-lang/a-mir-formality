#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../cfg.rkt"
         "../grammar.rkt"
         "dataflow.rkt"
         )
(provide loans-active-on-entry-to
         )

(define-metafunction formality-body
  loans-active-on-entry-to : Cfg Location -> LoanSet

  [(loans-active-on-entry-to Cfg Location)
   LoanSet
   (where/error [_ ... (Location LoanSet) _ ...] (active-loans-map Cfg))
   ]
  )

(define-metafunction formality-body
  active-loans-map : Cfg -> LocatedLoanSets

  [(active-loans-map Cfg)
   (dataflow active-loans Cfg LocatedLoanSets_initial)
   (where/error [Location_all ...] (cfg-locations Cfg))
   (where/error LocatedLoanSets_initial [(Location_all []) ...])
   ]
  )

(module+ test
  (redex-let*
   formality-body
   [(BasicBlockDecls (term [(bb0 {[(_2 = (ref ?0 () _1))    ; 4
                                   (fake-read _2)
                                   (_1 = (use (const 23)))
                                   ]
                                  return
                                  })]))
    (Cfg (term (control-flow-graph BasicBlockDecls)))
    ]

   (traced '()
           (test-equal
            (term (loans-active-on-entry-to Cfg (bb0 @ 0)))
            (term [])))

   (traced '()
           (test-equal
            (term (loans-active-on-entry-to Cfg (bb0 @ 1)))
            (term [(?0 () _1)])))

   (traced '()
           (test-equal
            (term (loans-active-on-entry-to Cfg (bb0 @ 2)))
            (term [(?0 () _1)])))
   )
  )
