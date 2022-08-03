#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         "places.rkt"
         )
(provide initial-move-set
         add-moved-place
         add-initialized-place
         union-move-sets
         place-is-fully-initialized?
         place-is-fully-moved?
         )

(define-metafunction formality-body
  ;; Creates a move set where all local variables are moved
  ;; (except for the arguments).
  initial-move-set : Γ -> MoveSet

  [(initial-move-set Γ)
   [LocalId_ret LocalId_other ...]
   (where/error [Ty_arg ..._arg] (argument-types-of-Γ Γ))
   (where/error [LocalDecl_ret LocalDecl_arg ..._arg LocalDecl_other ...] (local-decls-of-Γ Γ))
   (where/error (LocalId_ret _ _) LocalDecl_ret)
   (where/error [(LocalId_other _ _) ...] [LocalDecl_other ...])
   ]
  )

(define-metafunction formality-body
  ;; Union two move-sets.
  union-move-sets : MoveSet_0 MoveSet_1 -> MoveSet

  [(union-move-sets MoveSet_0 [])
   MoveSet_0
   ]

  [(union-move-sets MoveSet_0 [Place_0 Place_1 ...])
   (union-move-sets MoveSet_1 [Place_1 ...])
   (where/error MoveSet_1 (add-moved-place MoveSet_0 Place_0))
   ]

  )

(define-metafunction formality-body
  ;; Adjusts the move-set to account for `Place` having been moved
  ;; (i.e., adds `Place` to the set).
  ;;
  ;; If `Place` is already in the set, has no effect.
  add-moved-place : MoveSet Place -> MoveSet

  [(add-moved-place MoveSet Place)
   (place-set-add MoveSet Place)
   ]
  )

(define-metafunction formality-body
  ;; Adjusts the move-set to account for `Place` having been initialized
  ;; (i.e., removes `Place` from the set).
  ;;
  ;; If `Place` is not in the set, has no effect.
  add-initialized-place : MoveSet Place -> MoveSet

  [(add-initialized-place MoveSet Place)
   (place-set-remove-any-suffix MoveSet Place)]
  )

(define-metafunction formality-body
  place-is-fully-initialized? : MoveSet Place -> boolean

  [(place-is-fully-initialized? MoveSet Place)
   (not? (place-set-contains-intersecting-place? MoveSet Place))
   ]
  )

(define-metafunction formality-body
  place-is-fully-moved? : MoveSet Place -> boolean

  [(place-is-fully-moved? MoveSet Place)
   (place-set-contains-prefix-of? MoveSet Place)
   ]
  )