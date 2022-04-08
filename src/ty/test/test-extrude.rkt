#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../extrude.rkt"
         "../inequalities.rkt"
         "../../logic/instantiate.rkt"
         "../../logic/env.rkt"
         "../../util.rkt"
         )

(module+ test
  (redex-let*
   formality-ty


   (; Forall A, B. Forall X: X <= A, X <= B.
    ((Env_0 () (Ty_A Ty_B)) (term (instantiate-quantified EmptyEnv (ForAll ((TyKind A) (TyKind B)) ()))))
    (Universe_1 (term (universe-of-var-in-env Env_0 Ty_A)))
    ((Env_1 () (Ty_X)) (term (instantiate-quantified Env_0 (ForAll ((TyKind X)) ()))))
    (Env_2 (term (env-with-var-related-to-parameters Env_1 Ty_X <= (Ty_A Ty_B)))))

   (redex-let*
    formality-ty

    (; Ty_extruded <= X, but in U1 (same as A, B)
     ;
     ; Can't prove this, because we have no lower bounds on X.
     ((Env_3 Ty_extruded Goals) (term (extrude-parameter Env_2 Universe_1 <= Ty_X)))
     )

    (test-equal
     (term (universe-of-var-in-env Env_3 Ty_extruded))
     (term Universe_1))

    (test-equal
     (term (variable-bounds Env_3 Ty_extruded))
     (term (() ())))

    (; Goals are not provable.
     test-equal
     (term Goals)
     (term ((Any ()))))

    )

   (redex-let*
    formality-ty

    (; Ty_extruded >= X, but in U1 (same as A, B)
     ;
     ; True if `Ty_extruded` is greater than `A` or `B`.
     ((Env_3 Ty_extruded Goals) (term (extrude-parameter Env_2 Universe_1 >= Ty_X)))
     )

    (test-equal
     (term (universe-of-var-in-env Env_3 Ty_extruded))
     (term Universe_1))

    (test-equal
     (term (variable-bounds Env_3 Ty_extruded))
     (term (() ())))

    (test-equal
     (term Goals)
     (term ((Any ((Ty_extruded >= Ty_B) (Ty_extruded >= Ty_A))))))

    )

   )
  )