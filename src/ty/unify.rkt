#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "instantiate.rkt"
         "substitution.rkt"
         )
(provide most-general-unifier)

(define-metafunction formality-ty
  ;; Given an environment and a set `ParameterPairs` of `(Parameter Parameter)` pairs,
  ;; computes a new environment and a substitution from any type variables within that
  ;; set such that they are equal, or yields `Error` if there is no such set.
  ;; The new environment is the same as the input environment except that some
  ;; existential variables may have been moved to a smaller universe.
  ;;
  ;; The algorithm is a variant of the classic unification algorithm adapted for
  ;; universes. It is described in "A Proof Procedure for the Logic of Hereditary Harrop Formulas"
  ;; publishd in 1992 by Gopalan Nadathur at Duke University.
  most-general-unifier : Env ParameterPairs -> (Env Substitution) or Error

  [; base case, nothing to be done
   (most-general-unifier Env ())
   (Env ())
   ]

  [; X = X... ok.
   (most-general-unifier Env ((VarId VarId) ParameterPair_1 ...))
   (most-general-unifier Env (ParameterPair_1 ...))
   ]

  [; X = P: Replace [X => P] and seek a solution for the remaining pairs.
   ; If that errors... propagate.
   (most-general-unifier Env ((VarId Parameter) ParameterPair_1 ...))
   Error

   (where Error (variable-elimination Env VarId Parameter (ParameterPair_1 ...)))
   ]

  [; X = P: Replace [X => P] and seek a solution for the remaining pairs.
   ; If that succeeds, add [X => P] to the resulting substitution and then
   ; apply it to itself until a fixed point is reached.
   ;
   ; This substitution is called: variable elimination.
   (most-general-unifier Env ((VarId Parameter) ParameterPair_1 ...))
   (Env_mgu (substitution-fix Substitution_mgu))

   (where (Env_mgu ((VarId_mgu Parameter_mgu) ...)) (variable-elimination Env VarId Parameter (ParameterPair_1 ...)))
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

  [(most-general-unifier Env
                         (((TyApply TyName_!_0 Substitution_l) (TyApply TyName_!_0 Substitution_r))
                          ParameterPair_1 ...))
   Error]

  )

(define-metafunction formality-ty
  ;; Presuming that `VarId => Parameter` passes the occurs check,
  ;; substitutes `VarId => Parameter` in the remaining `ParameterPairs`
  ;; and computes the most-general-unifier of the result.
  variable-elimination : Env VarId Parameter ParameterPairs -> (Env Substitution) or Error

  [
   (variable-elimination Env VarId Parameter ParameterPairs)
   Error

   (where Error (occurs-check Env VarId Parameter))
   ]

  [
   (variable-elimination Env VarId Parameter ParameterPairs)
   (most-general-unifier Env_1 ParameterPairs_elim)

   (where Env_1 (occurs-check Env VarId Parameter))
   (where/error ParameterPairs_elim (substitute ParameterPairs VarId Parameter))
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
  ;; occurs-check tests
  (redex-let*
   formality-ty
   ((; T, U, and E are in U0
     Env_0 (term (env-with-vars-in-current-universe EmptyEnv (T U E))))
    (; V is a placeholder in U1
     (Env_1 Ty_V) (term (instantiate-quantified Env_0 ForAll ((TyVar V)) V)))
    (; X is in U1, too
     Env_2 (term (env-with-vars-in-current-universe Env_1 (X)))))

   ; Equating `E` with `i32` is OK
   (test-equal
    (term (occurs-check Env_1 E (TyApply i32 ())))
    (term Env_1))

   ; Equating `E` with `Vec<E>` is not possible
   (test-equal
    (term (occurs-check Env_1 E (TyApply Vec (E))))
    (term Error))

   ; Equating `E` with `Vec<i32>` is ok
   (test-equal
    (term (occurs-check Env_1 E (TyApply Vec (i32))))
    (term Env_1))

   ; Equating `E` with `V` is not possible,
   ; since `V` is in U1
   (test-equal
    (term (occurs-check Env_1 E Ty_V))
    (term Error))

   ; Equating `X` with `V` is ok, both are in U1
   (test-equal
    (term (occurs-check Env_2 X Ty_V))
    (term Env_2))

   ; Equating E with `Vec<V>` is not possible,
   ; since V is in U1
   (test-equal
    (term (occurs-check Env_1 E (TyApply Vec (Ty_V))))
    (term Error))

   ; Equating X (in U1) with E (in U0) moves X to U0
   (redex-let
    formality-ty
    [(Env_2 (term (occurs-check Env_2 E X)))]

    (test-equal
     (term (universe-of-var-in-env Env_2 E))
     (term RootUniverse))

    (test-equal
     (term (universe-of-var-in-env Env_2 X))
     (term RootUniverse))

    )
   )

  ;; most-general-unifier tests
  (redex-let*
   formality-ty
   ((; A, B, and C are existential variables in U0
     (Env_0 (Ty_A Ty_B Ty_C)) (term (instantiate-quantified EmptyEnv Exists ((TyVar A) (TyVar B) (TyVar C)) (A B C))))
    (; T, U, and V are placeholders in U1
     (Env_1 (Ty_T Ty_U Ty_V)) (term (instantiate-quantified Env_0 ForAll ((TyVar T) (TyVar U) (TyVar V)) (T U V))))
    (; X, Y, and Z are existential variables in U1
     (Env_2 (Ty_X Ty_Y Ty_Z)) (term (instantiate-quantified Env_1 Exists ((TyVar X) (TyVar Y) (TyVar Z)) (X Y Z)))))

   ; Test [Vec<X> = Vec<T>]
   ;
   ; yields [X => T]
   (redex-let*
    formality-ty
    (((Env_out Substitution_out) (term (most-general-unifier Env_2 (((TyApply Vec (Ty_X)) (TyApply Vec (Ty_T))))))))
    (test-equal (term Env_out) (term Env_2))
    (test-equal (term Substitution_out) (term ((Ty_X Ty_T))))
    )

   ; Test [Vec<A> = Vec<T>]
   ;
   ; yields error
   (test-equal (term (most-general-unifier Env_2 (((TyApply Vec (Ty_A)) (TyApply Vec (Ty_T))))))
               (term Error))

   ; Test [Vec<A> = Vec<X>, Vec<X> = Vec<T>] results in an error.
   (test-equal (term (most-general-unifier Env_2 (((TyApply Vec (Ty_A)) (TyApply Vec (Ty_X)))
                                                  ((TyApply Vec (Ty_X)) (TyApply Vec (Ty_T))))))
               (term Error))

   ; Same as above, but reversed in order.
   (test-equal (term (most-general-unifier Env_2 (((TyApply Vec (Ty_X)) (TyApply Vec (Ty_T)))
                                                  ((TyApply Vec (Ty_A)) (TyApply Vec (Ty_X))))))
               (term Error))


   ; Test [A = X, X = Vec<Y>, Y = i32]
   ;
   ; yields [A = Vec<i32>, X = Vec<i32>, Y = i32] and moves A, X, and Y
   ; into the root universe.
   (redex-let*
    formality-ty
    (((Env_out Substitution_out) (term (most-general-unifier Env_2 ((Ty_A Ty_X)
                                                                    (Ty_X (TyApply Vec (Ty_Y)))
                                                                    (Ty_Y (TyApply i32 ()))
                                                                    )))))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Ty_A)))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Ty_X)))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Ty_Y)))
    (test-equal (term Substitution_out) (term ((Ty_A (TyApply Vec ((TyApply i32 ()))))
                                               (Ty_X (TyApply Vec ((TyApply i32 ()))))
                                               (Ty_Y (TyApply i32 ()))))))

   )

  )
