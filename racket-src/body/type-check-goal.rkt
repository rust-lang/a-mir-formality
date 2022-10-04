#lang racket
(require redex/reduction-semantics
         "../logic/grammar.rkt"
         "../logic/env.rkt"
         "grammar.rkt"
         "locations.rkt"
         "type-check/type-of.rkt"
         "type-check/basic-block.rkt"
         "type-check/inputs-outputs.rkt"
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








