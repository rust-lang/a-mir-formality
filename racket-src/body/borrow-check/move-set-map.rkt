#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "move-set.rkt"
         )
(provide add-move-set-to-map
         union-move-set-maps
         )

(define-metafunction formality-body
  ;; Updates the move-set-map so that the paths in `MoveSet` are considered moved on entry to
  ;; `BasicBlockIds` (in addition to whatever paths were already considered moved).
  union-move-set-maps : MoveSetMap_0 MoveSetMap_1 -> MoveSetMap

  [(union-move-set-maps MoveSetMap_0 [])
   MoveSetMap_0
   ]

  [(union-move-set-maps MoveSetMap_0 [(BasicBlockId_0 MoveSet_0) (BasicBlockId_1 MoveSet_1) ...])
   (union-move-set-maps MoveSetMap_1 [(BasicBlockId_1 MoveSet_1) ...])
   (where/error MoveSetMap_1 (add-move-set-to-map-1 MoveSetMap_0 BasicBlockId_0 MoveSet_0))
   ]
  )

(define-metafunction formality-body
  ;; Updates the move-set-map so that the paths in `MoveSet` are considered moved on entry to
  ;; `BasicBlockIds` (in addition to whatever paths were already considered moved).
  add-move-set-to-map : MoveSetMap BasicBlockIds MoveSet -> MoveSetMap

  [(add-move-set-to-map MoveSetMap [] MoveSet_new)
   MoveSetMap
   ]

  [(add-move-set-to-map MoveSetMap [BasicBlockId_0 BasicBlockId_1 ...] MoveSet_new)
   (add-move-set-to-map MoveSetMap_1 [BasicBlockId_1 ...] MoveSet_new)
   (where/error MoveSetMap_1 (add-move-set-to-map-1 MoveSetMap BasicBlockId_0 MoveSet_new))
   ]
  )

(define-metafunction formality-body
  ;; Helper for the above that permits only a single basic block
  add-move-set-to-map-1 : MoveSetMap BasicBlockId MoveSet -> MoveSetMap

  [(add-move-set-to-map-1 MoveSetMap BasicBlockId MoveSet_new)
   [(BasicBlockId_0 MoveSet_0) ... (BasicBlockId MoveSet_u) (BasicBlockId_1 MoveSet_1) ...]
   (where/error [(BasicBlockId_0 MoveSet_0) ... (BasicBlockId MoveSet_old) (BasicBlockId_1 MoveSet_1) ...] MoveSetMap)
   (where/error MoveSet_u (union-move-sets MoveSet_old MoveSet_new))
   ]
  )