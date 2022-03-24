#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "instantiate.rkt"
         "substitution.rkt"
         "hook.rkt"
         "../util.rkt"
         )
(provide unify
         unify/vars)

(define-metafunction formality-ty
  ;; Given an environment `Env` and a set `TermPairs` of `(Term Term)` pairs,
  ;; computes a new environment `Env_out` whose substitution maps existential
  ;; type variables from `Env` as needed to ensure that each `(Term Term)` pair is
  ;; equal. Yields `Error` if there is no such set. As a side-effect of this process,
  ;; existential variables may also be moved to smaller universes (because `?X = ?Y` requires
  ;; that `?X` and `?Y` be in the same universe).
  ;;
  ;; The algorithm is a variant of the classic unification algorithm adapted for
  ;; universes. It is described in "A Proof Procedure for the Logic of Hereditary Harrop Formulas"
  ;; publishd in 1992 by Gopalan Nadathur at Duke University.
  unify : Env TermPairs -> Env or Error

  [(unify Env TermPairs)
   (unify-pairs VarIds_exists Env TermPairs_subst)

   (where/error TermPairs_subst (apply-substitution-from-env Env TermPairs))
   (where/error VarIds_exists (existential-vars-in-env Env))]
  )

(define-metafunction formality-ty
  ;; Like `unify`, but treats `VarIds` as variables and
  ;; everything else as fixed.
  unify/vars : VarIds Env TermPairs -> Env or Error

  [(unify/vars VarIds Env TermPairs)
   (unify-pairs VarIds Env TermPairs_subst)
   (where/error TermPairs_subst (apply-substitution-from-env Env TermPairs))]
  )

(define-metafunction formality-ty
  ;; Returns the `VarId -> Universe` mapping from the environment
  existential-vars-in-env : Env -> VarIds

  [(existential-vars-in-env Env)
   (existential-vars-from-binders VarBinders)
   (where/error VarBinders (env-var-binders Env))
   ]
  )

(define-metafunction formality-ty
  ;; Filters out universal (ForAll) binders and returns just the VarIds of the existential ones.
  existential-vars-from-binders : VarBinders -> VarIds

  [(existential-vars-from-binders ()) ()]

  [(existential-vars-from-binders ((_ ForAll _) VarBinder_2 ...))
   (existential-vars-from-binders (VarBinder_2 ...))]

  [(existential-vars-from-binders ((VarId_1 Exists _) VarBinder_2 ...))
   (VarId_1 VarId_2 ...)
   (where/error (VarId_2 ...) (existential-vars-from-binders (VarBinder_2 ...)))]

  )

(define-metafunction formality-ty
  unify-pairs : VarIds_exists Env TermPairs -> Env or Error
  [; base case, all done, but we have to apply the substitution to itself
   (unify-pairs VarIds_exists Env ())
   Env_1
   (where/error Substitution_1 (substitution-fix (env-substitution Env)))
   (where/error Env_1 (apply-substitution-to-env Substitution_1 Env))
   ]

  [; unify the first pair ===> if that is an error, fail
   (unify-pairs VarIds_exists Env (TermPair_first TermPair_rest ...))
   Error
   (where Error (unify-pair VarIds_exists Env TermPair_first))]

  [; unify the first pair ===> if that succeeds, apply resulting substitution to the rest
   ; and recurse
   (unify-pairs VarIds_exists Env (TermPair_first TermPair_rest ...))
   (unify-pairs VarIds_exists Env_u TermPairs_all)

   (where (Env_u (TermPair_u ...)) (unify-pair VarIds_exists Env TermPair_first))
   (where/error (TermPair_v ...) (apply-substitution-from-env Env_u (TermPair_rest ...)))
   (where/error TermPairs_all (TermPair_u ... TermPair_v ...))
   ]
  )

(define-metafunction formality-ty
  unify-pair : VarIds_exists Env TermPair -> (Env TermPairs) or Error

  [; X = X ===> always ok
   (unify-pair _ Env (Term Term))
   (Env ())
   ]

  [; X = P ===> occurs check ok, return `[X => P]`
   (unify-pair VarIds_exists Env (VarId Parameter))
   ((env-with-var-mapped-to Env_out VarId Parameter) ())

   (where #t (contains-id VarIds_exists VarId))
   (where Env_out (occurs-check Env VarId Parameter))
   ]

  [; X = P ===> but occurs check fails, return Error
   (unify-pair VarIds_exists Env (VarId Parameter))
   Error

   (where #t (contains-id VarIds_exists VarId))
   (where Error (occurs-check Env VarId Parameter))
   ]

  [; P = X ===> just reverse order
   (unify-pair VarIds_exists Env (Parameter VarId))
   (unify-pair VarIds_exists Env (VarId Parameter))

   (where #t (contains-id VarIds_exists VarId))
   ]

  [; Universal placeholders like `(! X)` and `(! Y)` must
   ; be syntactically equal to be unified.
   ;
   ; Note: It's important that we don't recurse and cmopare
   ; `!` to `!`, `X` to `Y` etc, because if we did so, we would
   ; mistake `X` and `Y` for existential variables (they are variables
   ; defined in the environment, and the environment doesn't presently
   ; distinguish *how* they are defined, for better or -- arguably -- worse).
   (unify-pair VarIds_exists Env ((! VarId_!_0) (! VarId_!_0)))
   Error
   ]

  [; (L ...) = (R ...) ===> true if Li = Ri for all i and the lengths are the same
   (unify-pair VarIds_exists Env ((Term_l ..._0)
                                  (Term_r ..._0)))
   (Env ((Term_l Term_r) ...))]

  [; any other case fails to unify
   (unify-pair VarIds_exists Env (Term_1 Term_2))
   Error
   ]

  )

(define-metafunction formality-ty
  contains-id : VarIds VarId -> boolean

  [(contains-id (_ ... VarId _ ...) VarId) #t]
  [(contains-id _ _) #f]
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
  occurs-check : Env VarId Parameter -> Env or Error

  [(occurs-check Env VarId Parameter)
   Env_1

   ; can't have X = Vec<X> or whatever, that would be infinite in size
   (where #f (appears-free VarId Parameter))

   ; find the universe of `VarId`
   (where/error Universe_VarId (universe-of-var-in-env Env VarId))

   ; can't have `X = T<>` if `X` cannot see the universe of `T`
   (where/error (VarId_placeholder ...) (placeholder-variables Parameter))
   (where #t (all? (universe-includes Universe_VarId (universe-of-var-in-env Env VarId_placeholder)) ...))

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
     Env_0 (term (env-with-vars-in-current-universe EmptyEnv Exists (T U E))))
    (; V is a placeholder in U1
     (Env_1 Ty_V _) (term (instantiate-quantified Env_0 (ForAll ((TyKind V)) V))))
    (; X is in U1, too
     Env_2 (term (env-with-vars-in-current-universe Env_1 Exists (X)))))

   ; Equating `E` with `i32` is OK
   (test-equal
    (term (occurs-check Env_1 E (TyRigid i32 ())))
    (term Env_1))

   ; Equating `E` with `Vec<E>` is not possible
   (test-equal
    (term (occurs-check Env_1 E (TyRigid Vec (E))))
    (term Error))

   ; Equating `E` with `Vec<i32>` is ok
   (test-equal
    (term (occurs-check Env_1 E (TyRigid Vec ((scalar-ty i32)))))
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
    (term (occurs-check Env_1 E (TyRigid Vec (Ty_V))))
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

  ;; unify tests
  (redex-let*
   formality-ty
   ((; A, B, and C are existential variables in U0
     (Env_0 (Ty_A Ty_B Ty_C) _) (term (instantiate-quantified EmptyEnv (Exists ((TyKind A) (TyKind B) (TyKind C)) (A B C)))))
    (; T, U, and V are placeholders in U1
     (Env_1 (Ty_T Ty_U Ty_V) _) (term (instantiate-quantified Env_0 (ForAll ((TyKind T) (TyKind U) (TyKind V)) (T U V)))))
    (; X, Y, and Z are existential variables in U1
     (Env_2 (Ty_X Ty_Y Ty_Z) _) (term (instantiate-quantified Env_1 (Exists ((TyKind X) (TyKind Y) (TyKind Z)) (X Y Z))))))

   ; Test [Vec<X> = Vec<T>]
   ;
   ; yields [X => T]
   (redex-let*
    formality-ty
    ((Env_out (term (unify Env_2 (((TyRigid Vec (Ty_X)) (TyRigid Vec (Ty_T))))))))
    (test-equal (term (env-var-binders Env_out)) (term (env-var-binders Env_2)))
    (test-equal (term (apply-substitution-from-env Env_out Ty_X)) (term Ty_T))
    )

   ; Test some random loop-y structures like (A B C) = (T U V)
   ;
   ; yields [X => T]
   (redex-let*
    formality-ty
    ((Env_out (term (unify Env_2 (((Ty_X Ty_Y Ty_Z)
                                   (Ty_T Ty_U Ty_V)))))))
    (test-equal (term (env-var-binders Env_out)) (term (env-var-binders Env_2)))
    (test-equal (term (apply-substitution-from-env Env_out (Ty_X Ty_Y Ty_Z)))
                (term (Ty_T Ty_U Ty_V))
                )
    )

   ; Test [Vec<A> = Vec<T>]
   ;
   ; yields error
   (test-equal (term (unify Env_2 (((TyRigid Vec (Ty_A)) (TyRigid Vec (Ty_T))))))
               (term Error))

   ; Test [Vec<A> = Vec<X>, Vec<X> = Vec<T>] results in an error.
   (test-equal (term (unify Env_2 (((TyRigid Vec (Ty_A)) (TyRigid Vec (Ty_X)))
                                   ((TyRigid Vec (Ty_X)) (TyRigid Vec (Ty_T))))))
               (term Error))


   ; Test (i32: Eq) != (i32: PartialEq)
   ;
   ; yields error
   (test-equal (term (unify Env_2 (((Eq ((TyRigid i32 ()))) (PartialEq ((TyRigid i32 ())))))))
               (term Error))

   ; Test [A = X, X = Vec<Y>, Y = i32]
   ;
   ; yields [A = Vec<i32>, X = Vec<i32>, Y = i32] and moves A, X, and Y
   ; into the root universe.
   (redex-let*
    formality-ty
    ((Env_out (term (unify Env_2 ((Ty_A Ty_X)
                                  (Ty_X (TyRigid Vec (Ty_Y)))
                                  (Ty_Y (TyRigid i32 ()))
                                  )))))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Ty_A)))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Ty_X)))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Ty_Y)))
    (test-equal (term (apply-substitution-from-env Env_out (Ty_A Ty_X Ty_Y)))
                (term ((TyRigid Vec ((TyRigid i32 ())))
                       (TyRigid Vec ((TyRigid i32 ())))
                       (TyRigid i32 ())))))

   (; Test that the substitution is applied to hypotheses in the environment, too
    redex-let*
    formality-ty
    ((; assume that `X: Debug` (note that `X` is an existential variable)
      Env_3 (term (env-with-hypotheses Env_2 ((Implemented (Debug (Ty_X)))))))
     (; constrain `X = i32` to yield new substitution
      Env_out (term (unify Env_3 ((Ty_X (scalar-ty i32)))))))

    ; concluded that `X = i32`
    (test-equal (term (apply-substitution-from-env Env_out Ty_X)) (term (scalar-ty i32)))

    ; starts out as `X: Debug`
    (test-equal (term (env-hypotheses Env_3)) (term ((Implemented (Debug (Ty_X))))))

    ; changes to `i32: Debug` now that we know `X = i32`
    (test-equal (term (env-hypotheses Env_out)) (term ((Implemented (Debug ((scalar-ty i32)))))))
    )

   ; Here we can only map X but that's fine.
   (test-equal (term (apply-substitution-from-env
                      (unify/vars (Ty_X) Env_2 ((Ty_X (scalar-ty i32))
                                                (Ty_X Ty_X)
                                                (Ty_X (scalar-ty i32))))
                      Ty_X))
               (term (scalar-ty i32)))

   ; Here, even though Y is a variable, we fail because we're not allowed to assign it.
   (test-equal (term (unify/vars (Ty_X) Env_2 ((Ty_X Ty_Y) (Ty_X (scalar-ty i32)))))
               (term Error))
   )
  )
