#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../cfg.rkt"
         "../grammar.rkt"
         "dataflow.rkt"
         "liveness-compute.rkt"
         )
(provide loans-active-on-entry-to
         )

(define-metafunction formality-body
  loans-active-on-entry-to : Γ Env Location -> LoanSet

  [(loans-active-on-entry-to Γ Env Location)
   LoanSet
   (where/error [_ ... (Location LoanSet) _ ...] (active-loans-map Γ Env))
   ]
  )

(define-metafunction formality-body
  active-loans-map : Γ Env -> LocatedLoanSets

  [(active-loans-map Γ Env)
   (dataflow (active-loans Γ Env LivenessAnalysis) Cfg LocatedLoanSets_initial)
   (where/error Cfg (control-flow-graph-from-Γ Γ))
   (where/error [Location_all ...] (cfg-locations Cfg))
   (where/error LocatedLoanSets_initial [(Location_all []) ...])
   (where/error LivenessAnalysis (liveness-analysis Cfg))
   ]
  )

#;(module+ test
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
