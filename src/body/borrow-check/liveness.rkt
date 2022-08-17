#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         "liveness-effects-of.rkt"
         "liveness-effects.rkt"
         )
(provide liveness
         )

(define-metafunction formality-body
  ;; Compute live variables on entry to each basic block.
  liveness : BasicBlockDecls -> LiveVariablesBeforeBlocks

  [(liveness BasicBlockDecls)
   (liveness-fix BasicBlockDecls LiveVariablesBeforeBlocks)

   (where/error [(BasicBlockId _) ...] BasicBlockDecls)
   (where/error LiveVariablesBeforeBlocks [(BasicBlockId (reads: [] drops: [])) ...])
   ]
  )

(define-metafunction formality-body
  ;; Compute liveness before each block by iterating until a fixed point is reached.
  liveness-fix : BasicBlockDecls LiveVariablesBeforeBlocks -> LiveVariablesBeforeBlocks

  [(liveness-fix BasicBlockDecls LiveVariablesBeforeBlocks)
   LiveVariablesBeforeBlocks
   (where LiveVariablesBeforeBlocks (liveness-step BasicBlockDecls LiveVariablesBeforeBlocks))
   ]

  [(liveness-fix BasicBlockDecls LiveVariablesBeforeBlocks)
   (liveness-fix BasicBlockDecls LiveVariablesBeforeBlocks_1)
   (where LiveVariablesBeforeBlocks_1 (liveness-step BasicBlockDecls LiveVariablesBeforeBlocks))
   ]
  )

(define-metafunction formality-body
  ;; Improve computation of liveness before each block by 1 step.
  liveness-step : BasicBlockDecls LiveVariablesBeforeBlocks -> LiveVariablesBeforeBlocks

  [(liveness-step BasicBlockDecls LiveVariablesBeforeBlocks)
   [(live-variables-before-block BasicBlockDecls LiveVariablesBeforeBlocks BasicBlockId) ...]
   (where/error [(BasicBlockId _) ...] LiveVariablesBeforeBlocks)
   ]
  )

(define-metafunction formality-body
  ;; Computes the live variables before the given block starts to execute,
  ;; given the current state of the live variables before each other block.
  live-variables-before-block : BasicBlockDecls LiveVariablesBeforeBlocks BasicBlockId -> LiveVariablesBeforeBlock

  [(live-variables-before-block BasicBlockDecls LiveVariablesBeforeBlocks BasicBlockId)
   (BasicBlockId (apply-effects-to-live-variables LivenessEffects_s LiveVariables_term))

   ; find the definition of this block
   (where/error [_ ... (BasicBlockId (Statements Terminator)) _ ...] BasicBlockDecls)

   ; compute the live variables before the terminator
   (where/error LiveVariables_term (live-variables-before-terminator LiveVariablesBeforeBlocks Terminator))

   ; compute effects of the statements
   (where/error LivenessEffects_s (liveness-effects-of-evaluating-statements Statements))
   ]

  )

(define-metafunction formality-body
  ;; Compute the live values before the given terminator.
  live-variables-before-terminator : LiveVariablesBeforeBlocks Terminator -> LiveVariables

  [(live-variables-before-terminator LiveVariablesBeforeBlocks Terminator)
   (union-live-variables LiveVariables ...)
   (where/error [TerminatorLivenessEffects ...] (liveness-effects-of-terminator Terminator))
   (where/error [LiveVariables ...] [(live-variables-before-successor LiveVariablesBeforeBlocks TerminatorLivenessEffects) ...])
   ]

  )

(define-metafunction formality-body
  ;; Computes the live variables before the terminator when coming from the given successor.
  live-variables-before-successor : LiveVariablesBeforeBlocks TerminatorLivenessEffects -> LiveVariables

  [(live-variables-before-successor LiveVariablesBeforeBlocks (BasicBlockId_succ LivenessEffects_term))
   (apply-effects-to-live-variables LivenessEffects_term LiveVariables_succ)

   ; find the read/drop-live values on entry to the succcessor
   (where/error [_ ... (BasicBlockId_succ LiveVariables_succ) _ ...] LiveVariablesBeforeBlocks)
   ]
  )

(define-metafunction formality-body
  ;; Given the liveness effects of some term, and the set of variables live after that term,
  ;; computes the variables live before.
  apply-effects-to-live-variables : LivenessEffects LiveVariables -> LiveVariables

  [(apply-effects-to-live-variables LivenessEffects (reads: LocalIds_r_in drops: LocalIds_d_in))
   (reads: LocalIds_r drops: LocalIds_d)

   (where/error (reads: LocalIds_r drops: LocalIds_d writes: _)
                (chain-liveness-effects LivenessEffects
                                        (reads: LocalIds_r_in drops: LocalIds_d_in writes: [])))
   ]
  )

(define-metafunction formality-body
  ;; Given the effect of two parallel paths, combine them.
  union-live-variables : LiveVariables_0 ... -> LiveVariables

  [(union-live-variables (reads: LocalIds_r_in drops: LocalIds_d_in) ...)
   (reads: LocalIds_r drops: LocalIds_d)

   ; a variable is read if ANY path reads it
   (where/error LocalIds_r (union-local-ids LocalIds_r_in ...))

   ; a variable is dropped if ANY path reads it (unless it is read, which is more general)
   (where/error LocalIds_d (minus-local-ids (union-local-ids LocalIds_d_in ...) LocalIds_r))
   ]
  )

(module+ test

  (test-equal
   (term (liveness [(bb0 { [(_0 = (use (copy _1))) ; _0 = _1
                            ]
                           return
                           })
                    ]))
   (term [(bb0 (reads: [_1] drops: []))
          ]))

  (; reading `_1` generates `_1` in the read set
   test-equal
   (term (liveness [(bb0 { [(_1 = (use (const 22)))
                            (_0 = (use (copy _1)))
                            ]
                           return
                           })
                    ]))
   (term [(bb0 (reads: [] drops: []))
          ]))

  (; writing to `_1` kills `_1`
   test-equal
   (term (liveness [(bb0 { [(_1 = (use (const 22)))
                            (_0 = (use (copy _1)))
                            ]
                           return
                           })
                    ]))
   (term [(bb0 (reads: [] drops: []))
          ]))

  (; writing to `_1.f` doesn't kill `_1`
   test-equal
   (term (liveness [(bb0 { [((field _1 f) = (use (const 22)))
                            (_0 = (use (copy _1)))
                            ]
                           return
                           })
                    ]))
   (term [(bb0 (reads: [_1] drops: []))
          ]))

  (; dropping `_0` generates a drop set
   test-equal
   (term (liveness [(bb0 { []
                           (drop _0 [bb1])
                           })

                    (bb1 { []
                           return
                           })
                    ]))
   (term [(bb0 (reads: [] drops: [_0]))
          (bb1 (reads: [] drops: []))
          ]))

  (; reading `_0` upgrades from drop
   test-equal
   (term (liveness [(bb0 { [(_1 = (use (copy _0)))
                            ]
                           (drop _0 [bb1])
                           })

                    (bb1 { []
                           return
                           })
                    ]))
   (term [(bb0 (reads: [_0] drops: []))
          (bb1 (reads: [] drops: []))
          ]))


  (; when we call `_0`, that is a write to `_1`,
   ; but only on the "ok" path
   test-equal
   (term (liveness [(bb0 { []
                           (call (copy some_func) [] _1 [bb1 bb2])
                           })

                    (bb1 { [(_0 = (use (copy _1)))]
                           return
                           })

                    (bb2 { []
                           return
                           })
                    ]))
   (term [(bb0 (reads: [some_func] drops: []))
          (bb1 (reads: [_1] drops: []))
          (bb2 (reads: [] drops: []))
          ]))

  (; when we call `_0`, that is a write to `_1`,
   ; but not on the error path
   test-equal
   (term (liveness [(bb0 { []
                           (call (copy some_func) [] _1 [bb1 bb2])
                           })

                    (bb1 { []
                           return
                           })

                    (bb2 { [(_0 = (use (copy _1)))]
                           return
                           })
                    ]))
   (term [(bb0 (reads: [_1 some_func] drops: []))
          (bb1 (reads: [] drops: []))
          (bb2 (reads: [_1] drops: []))
          ]))

  )