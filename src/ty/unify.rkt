; Based on "a proof procedure for "

#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "instantiate.rkt"
         "substitution.rkt"
         )
(provide most-general-unifier)

(define-metafunction formality-ty
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
   (where/error ParameterPairs_elim (substitute (ParameterPair_1 ...) VarId Parameter))
   (where Error (most-general-unifier Env ParameterPairs_elim))
   ]

  [; X = P: Replace [X => P] and seek a solution for the remaining pairs.
   ; If that succeeds, add [X => P] to the resulting substitution and then
   ; apply it to itself until a fixed point is reached.
   ;
   ; This substitution is called: variable elimination.
   (most-general-unifier Env ((VarId Parameter) ParameterPair_1 ...))
   (substitution-fix Substitution_mgu)

   (where/error ParameterPairs_elim (substitute (ParameterPair_1 ...) VarId Parameter))
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

(define-metafunction formality-ty
  ;; Checks whether `VarId` can be assigned the value of `Parameter`.
  ;;
  ;; Fails if:
  ;;
  ;; * `VarId` appears in `Parameter`
  ;; * `Parameter` mentions a placeholder that `VarId` cannot name because of its universe
  ;;
  ;; Returns a fresh environment which contains adjust universes for each
  ;; variable `V` that occur in `Parameter`. The universe of `V` cannot
  ;; be greater than the universe of `VarId` (since whatever value `V` ultimately
  ;; takes on will become part of `VarId`'s value).
  occurs-check : Env VarId Parameter -> Env-e

  [(occurs-check Env VarId Parameter)
   Env_1

   ; can't have X = Vec<X> or whatever, that would be infinite in size
   (where #f (appears-free VarId Parameter))

   ; find the universe of `VarId`
   (where/error Universe_VarId (universe-of-var-in-env Env VarId))

   ; can't have `X = T<>` if `X` cannot see the universe of `T`
   (where/error (VarId_placeholder ...) (placeholder-variables Parameter))
   (where #t (all? ((universe-includes Universe_VarId (universe-of-var-in-env Env VarId_placeholder)) ...)))

   ; for each `X = ... Y ...`, adjust universe of Y so that it can see all values of X
   (where/error VarIds_free (free-variables Parameter))
   (where/error Env_1 (env-with-vars-limited-to-universe Env VarIds_free Universe_VarId))
   ]

  [(occurs-check Env VarId Parameter)
   Error
   ]

  )

(module+ test

  (redex-let*
   formality-ty
   ((Env_0 (term (env-with-vars-in-current-universe EmptyEnv (T U E))))
    ((Env_1 Ty_V) (term (instantiate-quantified Env_0 ForAll ((TyVar V)) V))))

   (test-equal
    (term (occurs-check Env_1 E (TyApply i32 ())))
    (term Env_1))

   (test-equal
    (term (occurs-check Env_1 E (TyApply Vec (E))))
    (term Error))

   (test-equal
    (term (occurs-check Env_1 E (TyApply Vec (i32))))
    (term Env_1))

   (test-equal
    (term (occurs-check Env_1 E Ty_V))
    (term Error))

   (test-equal
    (term (occurs-check Env_1 E (TyApply Vec (Ty_V))))
    (term Error))
   )
  )
