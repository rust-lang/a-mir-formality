#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../cfg.rkt"
         "move-set.rkt"
         "dataflow.rkt"
         )
(provide infer-initialization
         )

(define-metafunction formality-body
  ;; Given a fn body, computes the initialization state on entry to each basic block using a
  ;; fixed-point, iterative process.
  ;;
  ;; Note that inference is intentionally separating from checking, which is found in `initialization-check`.
  ;; The idea is that inference is not "trusted" -- it infers a `MoveSetMap` that reflects what the program does,
  ;; but the program may still fail the check (and, if inference is broken, and e.g. identifies things as
  ;; initialized which are not, then the check should fail).
  infer-initialization : Γ -> MoveSetMap

  [(infer-initialization Γ)
   (dataflow moved-places Cfg MoveSetMap)

   ; Create the cfg.
   (where/error Cfg (control-flow-graph (basic-block-decls-of-Γ Γ)))

   ; Find start node of cfg
   (where/error ([(Location_start _) (Location_rest _) ...] _) Cfg)

   ; Create initial set of moved things at start -- everything but the parameters.
   ; Every other block starts with nothing moved, but we'll propagate bits into there as we go.
   (where/error MoveSet_all (initial-move-set Γ))
   (where/error MoveSetMap [(Location_start MoveSet_all)
                            (Location_rest []) ...])
   ]

  )
