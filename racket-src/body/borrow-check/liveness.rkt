#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../logic/env.rkt"
         "../../logic/env-inequalities.rkt"
         "../locations.rkt"
         "../cfg.rkt"
         "../grammar.rkt"
         )
(provide variable-live-on-entry-to
         )

(define-judgment-form
  formality-body
  #:mode (variable-live-on-entry-to I I I I)
  #:contract (variable-live-on-entry-to LivenessMode Cfg Location LocalId)

  [(where/error CfgNode (cfg-node-at Cfg Location))
   (where [_ ... (Location Location_succ) _ ...] (cfg-edges Cfg))
   (variable-not-assigned-on-path-from CfgNode Location_succ LocalId)
   (variable-live-on-entry-to LivenessMode Cfg Location_succ LocalId)
   ----------------------------------------
   (variable-live-on-entry-to LivenessMode Cfg Location LocalId)
   ]

  [(where/error CfgNode (cfg-node-at Cfg Location))
   (variable-read-by-node LivenessMode CfgNode Location LocalId)
   ----------------------------------------
   (variable-live-on-entry-to LivenessMode Cfg Location LocalId)
   ]
  )

(define-judgment-form
  formality-body
  #:mode (variable-not-assigned-on-path-from I I I)
  #:contract (variable-not-assigned-on-path-from CfgNode Location LocalId)

  [(where #f (in?/id LocalId (written-by-node CfgNode Location)))
   ----------------------------------------
   (variable-not-assigned-on-path-from CfgNode Location LocalId)
   ]

  )

(define-metafunction formality-body
  ;; Returns list of local-ids that are overwritten entirely by
  ;; `CfgNode` when it transitions to its successor `Location`.
  written-by-node : CfgNode Location -> LocalIds

  ; Calls only overwrite when passing to their first successor (the "ok" path).
  [(written-by-node (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId _ ...]) (BasicBlockId @ 0))
   (written-by-place Place_dest)
   ]

  ; Assignment statements are unconditional.
  [(written-by-node (Place = Rvalue) _)
   (written-by-place Place)
   ]

  [(written-by-node CfgNode Location)
   []
   ]

  )

(define-metafunction formality-body
  written-by-place : Place -> LocalIds

  ; writing to `x = foo` overwrites `x` entirely
  [(written-by-place LocalId)
   [LocalId]
   ]

  ; writing to `x.f = foo` means `x` is still live
  [(written-by-place CompoundPlace)
   []
   ]

  )

(define-judgment-form
  formality-body
  #:mode (variable-read-by-node I I I I)
  #:contract (variable-read-by-node LivenessMode CfgNode Location LocalId)

  [(where/error (reads: LocalIds drops: _) (effects-of-node CfgNode))
   (where #t (in?/id LocalId LocalIds))
   ----------------------------------------
   (variable-read-by-node use-live CfgNode Location LocalId)
   ]

  [(where/error (reads: _ drops: LocalIds) (effects-of-node CfgNode))
   (where #t (in?/id LocalId LocalIds))
   ----------------------------------------
   (variable-read-by-node drop-live CfgNode Location LocalId)
   ]

  )

(define-metafunction formality-body
  effects-of-node : CfgNode -> LivenessEffects

  [(effects-of-node (goto BasicBlockId))
   (reads: [] drops: [])
   ]

  [(effects-of-node resume)
   (reads: [] drops: [])
   ]

  [(effects-of-node abort)
   (reads: [] drops: [])
   ]

  [(effects-of-node return)
   (reads: [] drops: [])
   ]

  [(effects-of-node unreachable)
   (reads: [] drops: [])
   ]

  [(effects-of-node (drop Place [BasicBlockId ...]))
   (drop-place Place)
   ]

  [(effects-of-node (call Operand_fn [Operand_arg ...] Place_dest TargetIds))
   (union-liveness-effects (evaluate-operand Operand_fn)
                           (evaluate-operand Operand_arg) ...
                           )
   ]

  [(effects-of-node (Place = Rvalue))
   (union-liveness-effects (evaluate-rvalue Rvalue)
                           (write-place Place))
   ]

  [(effects-of-node (set-discriminant Place VariantId))
   (read-place Place)
   ]

  [(effects-of-node (storage-live LocalId))
   (reads: [] drops: [])
   ]

  [(effects-of-node (storage-dead LocalId))
   (reads: [] drops: [])
   ]

  [(effects-of-node noop)
   (reads: [] drops: [])
   ]

  [(effects-of-node (fake-read Place))
   (read-place Place)
   ]

  )

(define-metafunction formality-body
  evaluate-rvalue : Rvalue -> LivenessEffects

  [(evaluate-rvalue (use Operand))
   (evaluate-operand Operand)
   ]

  [(evaluate-rvalue (repeat Operand Constant))
   (evaluate-operand Operand)
   ]

  [(evaluate-rvalue (ref Lt MaybeMut Place))
   (read-place Place)
   ]

  [(evaluate-rvalue (addr-of MaybeMut Place))
   (read-place Place)
   ]

  [(evaluate-rvalue (len Place))
   (read-place Place)
   ]

  [(evaluate-rvalue (BinaryOp Operand_l Operand_r))
   (union-liveness-effects (evaluate-operand Operand_l)
                           (evaluate-operand Operand_r))
   ]

  )

(define-metafunction formality-body
  evaluate-operand : Operand -> LivenessEffects

  [(evaluate-operand (CopyMove Place))
   (read-place Place)
   ]

  [(evaluate-operand (const _))
   (reads: [] drops: [])
   ]
  )

(define-metafunction formality-body
  read-place : Place -> LivenessEffects

  [(read-place LocalId)
   (reads: [LocalId] drops: [])
   ]

  [(read-place (* Place))
   (read-place Place)
   ]

  [(read-place (field Place FieldId))
   (read-place Place)
   ]

  [(read-place (index Place LocalId_index))
   (union-liveness-effects (read-place Place)
                           (reads: [LocalId_index] drops: []))
   ]

  [(read-place (downcast Place VariantId))
   (read-place Place)
   ]
  )

(define-metafunction formality-body
  drop-place : Place -> LivenessEffects

  [(drop-place LocalId)
   (reads: [] drops: [LocalId])
   ]

  [(drop-place (* Place))
   (drop-place Place)
   ]

  [(drop-place (field Place FieldId))
   (drop-place Place)
   ]

  [(drop-place (index Place LocalId_index))
   (union-liveness-effects (drop-place Place)
                           (reads: [LocalId_index] drops: []))
   ]

  [(drop-place (downcast Place VariantId))
   (drop-place Place)
   ]
  )

(define-metafunction formality-body
  write-place : Place -> LivenessEffects

  [(write-place LocalId)
   (reads: [] drops: [])
   ]

  [(write-place Place)
   (read-place Place)
   ]
  )


(define-metafunction formality-body
  union-liveness-effects : LivenessEffects ... -> LivenessEffects

  [(union-liveness-effects LivenessEffects ...)
   (reads: (union-of LocalIds_r ...) drops: (union-of LocalIds_d ...))
   (where/error [(reads: LocalIds_r drops: LocalIds_d) ...] [LivenessEffects ...])
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
  #;(redex-let*
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
             (test-judgment-false (variable-live-on-entry-to use-live Cfg (bb0 @ 1) v1)))
     (traced '()
             (test-judgment-holds (variable-live-on-entry-to use-live Cfg (bb1 @ 0) v1)))
     )
  )
