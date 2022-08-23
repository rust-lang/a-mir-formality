#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "inequalities.rkt"
         "../logic/env.rkt"
         "../logic/flatten-hypothesis.rkt"
         "../logic/match.rkt"
         "../logic/substitution.rkt"
         )
(provide bound-placeholder-from-hypotheses
         )

(define-metafunction formality-ty
  ;; Returns a list of goals `Goal ...`, any one of which is sufficient to prove
  ;; that `!X ◃ P` based on the known bounds on `!X` found in the hypotheses.
  bound-placeholder-from-hypotheses : Env_in VarId_!X InequalityOp_◃ Parameter -> (Goal_out ...)

  #:pre (env-contains-placeholder-var Env_in VarId_!X)

  [(bound-placeholder-from-hypotheses Env VarId_!X InequalityOp_◃ Parameter)
   (Goal_out ... ...)
   (where/error (FlatHypothesis ...) (flattened-hypotheses-in-env Env))
   (where/error ((Goal_out ...) ...) ((match-hypothesis FlatHypothesis VarId_!X InequalityOp_◃ Parameter) ...))
   ]

  )

(define-metafunction formality-ty
  ;; Checks whether `FlatHypothesis` supplies a bound `B` where `B InequalityOp VarId`.
  ;; Returns either an empty list or a 1-element list if so.
  match-hypothesis : FlatHypothesis VarId InequalityOp_◃ Parameter -> (Goal_out ...)

  [; We are trying to show `VarId ◃ Parameter_b`
   (match-hypothesis FlatHypothesis VarId InequalityOp_◃ Parameter_b)
   ((simplify-goal Goal))

   (; We have a hypothesis
    ;
    ; ∀Xs_h. (G_h => (P_l ◃ P_r))
    where (∀ ((ParameterKind_h VarId_h) ...) (implies Goals_h (Parameter_l InequalityOp_◃ Parameter_r))) FlatHypothesis)
   (; find substitution Θ such that P_l = VarId
    ;
    ; Note that `domain(Θ) ⊆ Xs`
    where Substitution_Θ (match-terms (VarId_h ...) Parameter_l VarId))
   (; compute `Xs_r`, the variables in `Xs_h` that are not covered by `domain(Θ)`
    where/error KindedVarIds_r (remove-bound-variables ((ParameterKind_h VarId_h) ...) (substitution-domain Substitution_Θ)))
   (; let `Gs_h1 = Θ Gs_h`
    where/error (Goal_h1 ...) (apply-substitution Substitution_Θ Goals_h))
   (; let `P_r1 = Θ P_r`
    where/error Parameter_r1 (apply-substitution Substitution_Θ Parameter_r))
   (; the matched hypothesis `∀Xs_r. (Gs_h1 => (VarId ◃ P_r1))` shows that `VarId ◃ Parameter_b` if...
    where/error Goal (;∃ Xs_R
                      ∃ KindedVarIds_r
                        (&& (; Gs_h1 are satisfied
                             Goal_h1 ...
                             ; P_r1 ◃ Parameter_b
                             (Parameter_r1 InequalityOp_◃ Parameter_b)
                             ))))
   ]

  [(match-hypothesis (∀ ((ParameterKind_h VarId_h) ...) (implies Goals_h (Parameter_l InequalityOp_◃ Parameter_r))) VarId_v InequalityOp_▹ Parameter_b)
   (match-hypothesis (∀ ((ParameterKind_h VarId_h) ...) (implies Goals_h (Parameter_r InequalityOp_▹ Parameter_l))) VarId_v InequalityOp_▹ Parameter_b)
   (where InequalityOp_◃ (invert-inequality-op InequalityOp_▹))
   ]

  [(match-hypothesis _ _ _ _)
   ()
   ]

  )

(define-metafunction formality-ty
  ;; Remove any variables from `KindedVarIds` that appear in `VarIds`
  remove-bound-variables : KindedVarIds VarIds -> KindedVarIds

  [(remove-bound-variables (KindedVarId ...) VarIds_remove)
   (KindedVarId_out ... ...)
   (where/error ((KindedVarId_out ...) ...) ((remove-bound-variables-1 KindedVarId VarIds_remove) ...))
   ]
  )

(define-metafunction formality-ty
  ;; If `KindedVarId` appears in `VarIds`, return `()`,
  ;; else return `(KindedVarId)`. (Helper for `remove-bound-variables`.)
  remove-bound-variables-1 : KindedVarId VarIds -> KindedVarIds

  [(remove-bound-variables-1 (ParameterKind VarId) VarIds_remove)
   ()
   (where #t (in?/id VarId VarIds_remove))]

  [(remove-bound-variables-1 (ParameterKind VarId) VarIds_remove)
   ((ParameterKind VarId))
   (where #f (in?/id VarId VarIds_remove))]

  )

(define-metafunction formality-ty
  simplify-goal : Goal -> Goal

  [(simplify-goal (∃ () Goal))
   (simplify-goal Goal)]

  [(simplify-goal (&& (Goal)))
   (simplify-goal Goal)]

  [(simplify-goal (|| (Goal)))
   (simplify-goal Goal)]

  [(simplify-goal (implies () Goal))
   (simplify-goal Goal)]

  [(simplify-goal Goal)
   Goal]

  )
