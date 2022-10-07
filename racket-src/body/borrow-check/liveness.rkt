#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../logic/env.rkt"
         "../cfg.rkt"
         "../grammar.rkt"
         "dataflow.rkt"
         )
(provide variable-live-on-entry-to
         variables-live-on-entry-to
         )

(define-judgment-form
  formality-body
  #:mode (variable-live-on-entry-to I I I O)
  #:contract (variable-live-on-entry-to LivenessMode Cfg Location LocalId)


  [(where/error [_ ... LocalId_live _ ...] (variables-live-on-entry-to LivenessMode Cfg Location ))
   ----------------------------------------
   (variable-live-on-entry-to LivenessMode Cfg Location LocalId_live)
   ]

  )

(define-metafunction formality-body
  variables-live-on-entry-to : LivenessMode Cfg Location -> LocalIds

  [(variables-live-on-entry-to LivenessMode Cfg Location)
   LocalIds_live
   (where/error [_ ... (Location LocalIds_live) _ ...] (live-variables-map LivenessMode Cfg))
   ]
  )
(define-metafunction formality-body
  live-variables-map : LivenessMode Cfg -> LiveVariablesMap

  [(live-variables-map LivenessMode Cfg)
   (dataflow LivenessMode Cfg LiveVariablesMap_empty)
   (where/error [Location_all ...] (cfg-locations Cfg))
   (where/error LiveVariablesMap_empty [(Location_all []) ...])
   ]
  )

(module+ test
  ; Writing to v1 makes it not live
  (redex-let*
   formality-body
   [(BasicBlockDecls (term [(bb0 { [(v1 = (use (const 22)))
                                    (v0 = (use (copy v1)))
                                    ]
                                   return
                                   })
                            ]))
    (Cfg (term (control-flow-graph BasicBlockDecls)))
    ]

   ; False on entry to the block, because v1 is assigned
   (traced '()
           (test-judgment-false (variable-live-on-entry-to use-live Cfg (bb0 @ 0) v1)))
   ; True at second statement
   (traced '()
           (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb0 @ 1) v1)))
   )

  ; Writing to `v1.f` doesn't kill v1
  (redex-let*
   formality-body
   [(BasicBlockDecls (term [(bb0 { [((field v1 f) = (use (const 22)))
                                    (v0 = (use (copy v1)))
                                    ]
                                   return
                                   })
                            ]))
    (Cfg (term (control-flow-graph BasicBlockDecls)))
    ]

   (traced '()
           (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb0 @ 0) v1)))
   )

  ; Dropping makes things drop-live, not use-live
  (redex-let*
   formality-body
   [(BasicBlockDecls (term [(bb0 { []
                                   (drop v0 [bb1])
                                   })

                            (bb1 { []
                                   return
                                   })
                            ]))
    (Cfg (term (control-flow-graph BasicBlockDecls)))
    ]

   (traced '()
           (test-judgment-false (variable-live-on-entry-to use-live Cfg (bb0 @ 0) v0)))
   (traced '()
           (test-judgment-holds (variable-live-on-entry-to drop-live Cfg (bb0 @ 0) v0)))
   )

  ; Function calls write their destination
  (redex-let*
   formality-body
   [(BasicBlockDecls (term [(bb0 { []
                                   (call (copy some_func) [] v1 [bb1 bb2])
                                   })

                            (bb1 { [(v0 = (use (copy v1)))]
                                   return
                                   })

                            (bb2 { []
                                   return
                                   })
                            ]))
    (Cfg (term (control-flow-graph BasicBlockDecls)))
    ]

   (traced '()
           (test-judgment-false (variable-live-on-entry-to use-live Cfg (bb0 @ 0) v1)))
   (traced '()
           (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb1 @ 0) v1)))
   )

  ; Function calls write their destination, but only on success
  (redex-let*
   formality-body
   [(BasicBlockDecls (term [(bb0 { []
                                   (call (copy some_func) [] v1 [bb1 bb2])
                                   })

                            (bb1 { [(v0 = (use (copy v1)))]
                                   return
                                   })

                            (bb2 { [(v0 = (use (copy v1)))]
                                   return
                                   })
                            ]))
    (Cfg (term (control-flow-graph BasicBlockDecls)))
    ]

   (traced '()
           (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb0 @ 0) v1)))
   (traced '()
           (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb1 @ 0) v1)))
   )

  ; loop
  (redex-let*
   formality-body
   [(BasicBlockDecls (term [(bb0 { [(v1 = (use (const 22)))]
                                   (goto bb1)
                                   })

                            (bb1 { [(v0 = (use (copy v1)))]
                                   (goto bb1)
                                   })

                            (bb2 { []
                                   return
                                   })
                            ]))
    (Cfg (term (control-flow-graph BasicBlockDecls)))
    ]

   (traced '()
           (test-judgment-false (variable-live-on-entry-to use-live Cfg (bb0 @ 0) v1)))
   (traced '()
           (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb0 @ 1) v1)))
   (traced '()
           (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb1 @ 0) v1)))
   )
  )
