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
  occurs-check : Env VarId Parameter -> Env-e

  [(occurs-check Env VarId Parameter)
   Env_1

   (side-condition (not (term (appears-free VarId Parameter))))
   (where (Exists Universe_VarId) (binding-in-env Env VarId))
   (where/error VarIds_free (free-variables Parameter))
   (where Env_1 (occurs-check-fold-env Env VarIds_free Universe_VarId))
   ]

  [(occurs-check Env VarId Parameter)
   Error
   ]

  )

(define-metafunction patina-ty
  occurs-check-fold-env : Env VarIds Universe_new -> Env-e

  [(occurs-check-fold-env Env () Universe_new)
   Env]

  [(occurs-check-fold-env Env (VarId_0 VarId_1 ...) Universe_new)
   Error
   (where Error (occurs-check-env Env VarId_0 Universe_new))]

  [(occurs-check-fold-env Env (VarId_0 VarId_1 ...) Universe_new)
   (occurs-check-fold-env Env_1 (VarId_1 ...) Universe_new)
   (where Env_1 (occurs-check-env Env VarId_0 Universe_new))]
  )

(define-metafunction patina-ty
  occurs-check-env : Env VarId Universe_new -> Env-e

  [(occurs-check-env Env VarId Universe_new)
   Env
   (where Universe_old (universe-of-binding-in-env Env VarId))
   (where #t (universe-can-see Universe_new Universe_old))]

  [(occurs-check-env Env VarId Universe_new)
   (env-with-rebound-universe Env Universe_new)
   (where (Exists Universe_old) (binding-in-env Env VarId))
   (side-condition (not (term (universe-can-see Universe_new Universe_old))))
   ]

  [(occurs-check-env Env VarId Parameter)
   Error
   (where (ForAll Universe_old) (binding-in-env Env VarId))
   (side-condition (not (term (universe-can-see Universe_new Universe_old))))
   ]

  )

(module+ test

  (redex-let*
   patina-ty
   ((Env (term (env-with-fresh-binding EmptyEnv))))
   (test-equal (term Env) (term Env))
   )

  )
