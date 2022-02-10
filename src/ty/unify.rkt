; Based on "a proof procedure for "

#lang racket
(require redex/reduction-semantics "substitution.rkt" "grammar.rkt")
(provide most-general-unifier)

(define-metafunction patina-ty
  ; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ; fresh names.
  most-general-unifier : Env ParameterPairs -> Substitution-e

  [; X = X... ok.
   (most-general-unifier Env ((VarId VarId) ParameterPair_1 ...))
   (most-general-unifier Env (ParameterPair_1 ...))
   ]

  [; X = P: Replace [X => P] and seek a solution for the remaining pairs.
   ; If that errors... propagate.
   (most-general-unifier Env ((VarId Parameter) ParameterPair_1 ...))
   Error
   (where ParameterPairs_elim (substitute (ParameterPair_1 ...) VarId Parameter))
   (where Error (most-general-unifier Env ParameterPairs_elim))
   ]

  [; X = P: Replace [X => P] and seek a solution for the remaining pairs.
   ; If that succeeds, add [X => P] to the resulting substitution and then
   ; apply it to itself until a fixed point is reached.
   ;
   ; This substitution is called: variable elimination.
   (most-general-unifier Env ((VarId Parameter) ParameterPair_1 ...))
   (substitution-fix Substitution_mgu)
   (where ParameterPairs_elim (substitute (ParameterPair_1 ...) VarId Parameter))
   (where ((VarId_mgu Parameter_mgu) ...) (most-general-unifier Env ParameterPairs_elim))
   (where Substitution_mgu ((VarId Parameter) (VarId_mgu Parameter_mgu) ...))
   ]

  [(most-general-unifier Env ((Parameter VarId) ParameterPair_1 ...))
   (most-general-unifier Env ((VarId Parameter) ParameterPair_1 ...))
   ]

  [(most-general-unifier Env
                         (((TyApply TyName (Parameter_l ...)) (TyApply TyName (Parameter_r ...)))
                          ParameterPair_1 ...))
   (most-general-unifier Env ((Parameter_l Parameter_r) ... ParameterPair_1 ...))
   ]

  [(most-general-unifier Env (((TyApply TyName_!_0 Substitution_l) (TyApply TyName_!_0 Substitution_r))
                              ParameterPair_1 ...))
   Error]

  [(most-general-unifier Env (((TyApply TyName (Parameter_l ...)) (TyApply TyName (Parameter_r ...)))
                              ParameterPair_1 ...))
   (most-general-unifier Env ((Parameter_l Parameter_r) ... ParameterPair_1 ...))
   ]
  )

(define-metafunction patina-ty
  ; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ; fresh names.
  occurs-check-ok : Env VarId Parameter -> boolean

  [(occurs-check-ok Env VarId Parameter)
   true
   (side-condition (appears-free VarId Parameter))
   (where (Exists Universe_VarId) (var-binding-in-env Env VarId))
   ]
  )


(module+ test
  )
