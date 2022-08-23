#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "move-set.rkt"
         "move-set-map.rkt"
         "initialization-update.rkt"
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
   (infer-initialization-fix BasicBlockDecls MoveSetMap)

   ; create an initial map where all local variables are moved on entry,
   ; and everything else is empty.
   (where/error BasicBlockDecls (basic-block-decls-of-Γ Γ))
   (where/error [(BasicBlockId_0 BasicBlockData_0) (BasicBlockId_1 BasicBlockData_1) ...] BasicBlockDecls)
   (where/error MoveSet_all (initial-move-set Γ))
   (where/error MoveSetMap [(BasicBlockId_0 MoveSet_all)
                            (BasicBlockId_1 []) ...
                            ])
   ]
  )

(define-metafunction formality-body
  ;; Infers the initialization state on entry to each block, iterating
  ;; until a fixed point is reached.
  infer-initialization-fix : BasicBlockDecls MoveSetMap -> MoveSetMap

  [; Fixed point reached?
   (infer-initialization-fix BasicBlockDecls MoveSetMap)
   MoveSetMap
   (where MoveSetMap (infer-initialization-for-each-block BasicBlockDecls MoveSetMap))
   ]

  [; Iterate
   (infer-initialization-fix BasicBlockDecls MoveSetMap)
   (infer-initialization-fix BasicBlockDecls MoveSetMap_1)
   (where MoveSetMap_1 (infer-initialization-for-each-block BasicBlockDecls MoveSetMap))
   ]
  )

(define-metafunction formality-body
  ;; Updates the initialization state for the successors of basic blocks based on that
  ;; block's current input state and returns a new `MoveSetMap`.
  infer-initialization-for-each-block : BasicBlockDecls MoveSetMap -> MoveSetMap

  [(infer-initialization-for-each-block [] MoveSetMap)
   MoveSetMap
   ]

  [(infer-initialization-for-each-block [BasicBlockDecl_0 BasicBlockDecl_1 ...] MoveSetMap)
   (infer-initialization-for-each-block [BasicBlockDecl_1 ...] MoveSetMap_0)
   (where/error MoveSetMap_0 (infer-initialization-for-block BasicBlockDecl_0 MoveSetMap))
   ]
  )

(define-metafunction formality-body
  ;; Updates the initialization state for a single basic block.
  ;; Returns a new `MoveSetMap` where the state for the successors
  ;; of this block has been updated appropriately.
  infer-initialization-for-block : BasicBlockDecl MoveSetMap -> MoveSetMap

  [(infer-initialization-for-block (BasicBlockId (Statements Terminator)) MoveSetMap_in)
   (union-move-set-maps MoveSetMap_in MoveSetMap_out)

   ; find the current move-set on entry to the block
   (where/error [_ ... (BasicBlockId MoveSet_0) _ ...] MoveSetMap_in)

   ; account for the statements
   (where/error ([(MoveSet_s Statement) ...] MoveSet_t) (move-sets-for-statements MoveSet_0 Statements))

   ; account for the terminator
   (where/error MoveSetMap_out (move-sets-for-terminator MoveSet_t Terminator))
   ]
  )
