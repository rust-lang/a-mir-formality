#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../substitution.rkt"
         "../hook.rkt"
         "../env.rkt"
         "../instantiate.rkt"
         "hook.rkt"
         "unify.rkt")

; This file cannot be part of unify.rkt because we get a cycle error
; importing instantiate.rkt, since unify.rkt is used by the test module
; of everything else
(module+ test

  ;; occurs-check tests
  (redex-let*
   formality-logic
   ((; T, U, and E are in U0
     Env_0 (term (env-with-vars-in-current-universe EmptyEnv ∃ ((type T) (type U) (type E)))))
    (; V is a placeholder in U1
     (Env_1 Term_V _) (term (instantiate-quantified Env_0 (∀ ((type V)) V))))
    (; X is in U1, too
     Env_2 (term (env-with-vars-in-current-universe Env_1 ∃ ((type X))))))

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
    (term (occurs-check Env_1 E Term_V))
    (term Error))

   ; Equating `X` with `V` is ok, both are in U1
   (test-equal
    (term (occurs-check Env_2 X Term_V))
    (term Env_2))

   ; Equating E with `Vec<V>` is not possible,
   ; since V is in U1
   (test-equal
    (term (occurs-check Env_1 E (TyRigid Vec (Term_V))))
    (term Error))

   ; Equating X (in U1) with E (in U0) moves X to U0
   (redex-let
    formality-logic
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
   formality-logic
   ((; A, B, and C are existential variables in U0
     (Env_0 (Term_A Term_B Term_C) _) (term (instantiate-quantified EmptyEnv (∃ ((type A) (type B) (type C)) (A B C)))))
    (; T, U, and V are placeholders in U1
     (Env_1 (Term_T Term_U Term_V) _) (term (instantiate-quantified Env_0 (∀ ((type T) (type U) (type V)) (T U V)))))
    (; X, Y, and Z are existential variables in U1
     (Env_2 (Term_X Term_Y Term_Z) _) (term (instantiate-quantified Env_1 (∃ ((type X) (type Y) (type Z)) (X Y Z))))))

   ; Test [Vec<X> = Vec<T>]
   ;
   ; yields [X => T]
   (redex-let*
    formality-logic
    ((Env_out (term (unify Env_2 (((TyRigid Vec (Term_X)) (TyRigid Vec (Term_T))))))))
    (test-equal (term (env-var-binders Env_out)) (term (env-var-binders Env_2)))
    (test-equal (term (apply-substitution-from-env Env_out Term_X)) (term Term_T))
    )

   ; Test some random loop-y structures like (A B C) = (T U V)
   ;
   ; yields [X => T]
   (redex-let*
    formality-logic
    ((Env_out (term (unify Env_2 (((Term_X Term_Y Term_Z)
                                   (Term_T Term_U Term_V)))))))
    (test-equal (term (env-var-binders Env_out)) (term (env-var-binders Env_2)))
    (test-equal (term (apply-substitution-from-env Env_out (Term_X Term_Y Term_Z)))
                (term (Term_T Term_U Term_V))
                )
    )

   ; Test [Vec<A> = Vec<T>]
   ;
   ; yields error
   (test-equal (term (unify Env_2 (((TyRigid Vec (Term_A))
                                    (TyRigid Vec (Term_T))))))
               (term Error))

   ; Test [Vec<A> = Vec<X>, Vec<X> = Vec<T>] results in an error.
   (test-equal (term (unify Env_2 (((TyRigid Vec (Term_A)) (TyRigid Vec (Term_X)))
                                   ((TyRigid Vec (Term_X)) (TyRigid Vec (Term_T))))))
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
    formality-logic
    ((Env_out (term (unify Env_2 ((Term_A Term_X)
                                  (Term_X (TyRigid Vec (Term_Y)))
                                  (Term_Y (TyRigid i32 ()))
                                  )))))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Term_A)))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Term_X)))
    (test-equal (term RootUniverse) (term (universe-of-var-in-env Env_out Term_Y)))
    (test-equal (term (apply-substitution-from-env Env_out (Term_A Term_X Term_Y)))
                (term ((TyRigid Vec ((TyRigid i32 ())))
                       (TyRigid Vec ((TyRigid i32 ())))
                       (TyRigid i32 ())))))

   (; Test that the substitution is applied to hypotheses in the environment, too
    redex-let*
    formality-logic
    ((; assume that `X: Debug` (note that `X` is an existential variable)
      Env_3 (term (env-with-hypotheses Env_2 ((is-implemented (Debug (Term_X)))))))
     (; constrain `X = i32` to yield new substitution
      Env_out (term (unify Env_3 ((Term_X (scalar-ty i32)))))))

    ; concluded that `X = i32`
    (test-equal (term (apply-substitution-from-env Env_out Term_X)) (term (scalar-ty i32)))

    ; starts out as `X: Debug`
    (test-equal (term (env-hypotheses Env_3)) (term ((is-implemented (Debug (Term_X))))))

    ; changes to `i32: Debug` now that we know `X = i32`
    (test-equal (term (env-hypotheses Env_out)) (term ((is-implemented (Debug ((scalar-ty i32)))))))
    )
   )
  )