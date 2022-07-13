#lang racket
(require redex/reduction-semantics
         "../logic/grammar.rkt"
         "../logic/env.rkt"
         "grammar.rkt"
         "locations.rkt"
         "type-of.rkt"
         )
(provide type-check-goal/Γ
         )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Γ I O)
  #:contract (type-check-goal/Γ Γ GoalAtLocations)

  [(where/error (LocalDecls (BasicBlockDecl ...)) (locals-and-blocks-of-Γ Γ))
   (type-check-goal/BasicBlockDecl Γ BasicBlockDecl GoalAtLocations_bb) ...
   (type-check-goal/inputs-and-outputs Γ GoalAtLocations_io)
   ----------------------------------------
   (type-check-goal/Γ Γ (flatten (GoalAtLocations_bb ... GoalAtLocations_io)))
   ]
  )

;; for<'a, 'b> fn(&'a f32, &'b i32) -> &'b u32
;;
;; exists<'?0, '?1, '?2> {
;; Local:
;;    L0: &'?0 u32 "return slot"
;;    L1: &'?1 f32
;;    L2: &'?2 i32
;;    ...
;;    Ln: &'?n i32
;; }

(define-judgment-form
  formality-body
  #:mode (type-check-goal/inputs-and-outputs I O)
  #:contract (type-check-goal/inputs-and-outputs Γ GoalAtLocations)

  [; FIXME: check that the "return local" has the same type as the one given in Γ
   ;
   ; FIXME: check that the types from the signature in Γ correspond to the types of the
   ; corresponding local variables
   ;
   ; FIXME: check that the where-clauses in signature are well-formed?
   ----------------------------------------
   (type-check-goal/inputs-and-outputs Γ ())
   ]
  )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/BasicBlockDecl I I O)
  #:contract (type-check-goal/BasicBlockDecl Γ BasicBlockDecl GoalAtLocations)

  [(where/error (((Location_s Statement) ...) (Location_t Terminator)) (basic-block-locations BasicBlockDecl))
   (type-check-goal/Statement Γ Statement Goal_s) ...
   (type-check-goal/Terminator Γ Terminator Goal_t)
   ----------------------------------------
   (type-check-goal/BasicBlockDecl Γ
                                   BasicBlockDecl
                                   ((Location_s Goal_s) ... (Location_t Goal_t)))
   ]
  )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Statement I I O)
  #:contract (type-check-goal/Statement Γ Statement Goal)

  [(type-of/Place Γ Place Ty_place)
   (type-of/Rvalue Γ Rvalue Ty_rvalue)
   #;(mutability/Place Γ Place mut)
   ----------------------------------------
   (type-check-goal/Statement Γ
                              (Place = Rvalue)
                              (Ty_rvalue <= Ty_place))
   ]

  )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Terminator I I O)
  #:contract (type-check-goal/Terminator Γ Terminator Goal)

  [----------------------------------------
   (type-check-goal/Terminator Γ (goto BasicBlockId) true-goal)
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ resume true-goal)
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ abort true-goal)
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ return true-goal)
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ unreachable true-goal)
   ]

  [(type-of/Operand Γ Operand_fn Ty_fn)
   (type-of/Operand Γ Operand_arg Ty_arg) ...
   (type-of/Place Γ Place_dest Ty_dest)
   (where (∀ KindedVarIds (implies (Biformula ...) ((Ty_formal ...) -> Ty_ret))) (ty-signature Γ Ty_fn))
   (where Goal (∃ KindedVarIds (&& ((Ty_arg <= Ty_formal) ...
                                    (Ty_ret <= Ty_dest)
                                    Biformula ...
                                    ))))
   ----------------------------------------
   (type-check-goal/Terminator Γ (call Operand_fn (Operand_arg ...) Place_dest TargetIds) Goal)
   ]

  )

