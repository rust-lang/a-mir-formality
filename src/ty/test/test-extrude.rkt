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

  (redex-let*
   formality-ty

   (; ForAll. Exists A, B. ForAll. Exists X: X <= A, X <= B.
    ((Env_0 () ()) (term (instantiate-quantified EmptyEnv (ForAll () ()))))
    ((Env_1 () (Ty_A Ty_B)) (term (instantiate-quantified Env_0 (Exists ((TyKind A) (TyKind B)) ()))))
    (Universe_1 (term (universe-of-var-in-env Env_1 Ty_A)))
    ((Env_2 () ()) (term (instantiate-quantified Env_1 (ForAll () ()))))
    ((Env_3 () (Ty_X)) (term (instantiate-quantified Env_2 (Exists ((TyKind X)) ()))))
    (Env_4 (term (env-with-var-related-to-parameters Env_3 Ty_X <= (Ty_A Ty_B)))))

   (redex-let*
    formality-ty

    (; Ty_extruded <= X, but in U1 (same as A, B)
     ;
     ; Get `Ty_extruded <= A, B` and `X >= Ty_extruded`
     ((Env_5 Ty_extruded Goals) (term (extrude-parameter Env_4 Universe_1 <= Ty_X)))
     )

    (test-equal
     (term (universe-of-var-in-env Env_5 Ty_extruded))
     (term Universe_1))

    (test-equal
     (term (variable-bounds Env_5 Ty_extruded))
     (term (() (Ty_A Ty_B))))

    (test-equal
     (term (variable-bounds Env_5 Ty_X))
     (term ((Ty_extruded) (Ty_B Ty_A))))

    (test-equal
     (term Goals)
     (term ()))

    )

   (redex-let*
    formality-ty

    (; Ty_extruded >= X, but in U1 (same as A, B)
     ;
     ; Wind up with
     ;
     ; X <= Ty_extruded, A, B
     ((Env_5 Ty_extruded Goals) (term (extrude-parameter Env_4 Universe_1 >= Ty_X)))
     )

    (test-equal
     (term (universe-of-var-in-env Env_5 Ty_extruded))
     (term Universe_1))

    (test-equal
     (term (variable-bounds Env_5 Ty_extruded))
     (term (() ())))

    (test-equal
     (term (variable-bounds Env_5 Ty_X))
     (term (() (Ty_extruded Ty_B Ty_A))))

    (test-equal
     (term Goals)
     (term ()))

    )

   )

  (redex-let*
   formality-ty

   (; ForAll. Exists A, B. ForAll. Exists X, Y: X <= Y, Y <= A, Y <= B
    ((Env_0 () ()) (term (instantiate-quantified EmptyEnv (ForAll () ()))))
    ((Env_1 () (Ty_A Ty_B)) (term (instantiate-quantified Env_0 (Exists ((TyKind A) (TyKind B)) ()))))
    (Universe_1 (term (universe-of-var-in-env Env_1 Ty_A)))
    ((Env_2 () ()) (term (instantiate-quantified Env_1 (ForAll () ()))))
    ((Env_3 () (Ty_X Ty_Y)) (term (instantiate-quantified Env_2 (Exists ((TyKind X) (TyKind Y)) ()))))
    (Env_4 (term (env-with-var-related-to-parameters Env_3 Ty_X <= (Ty_Y))))
    (Env_5 (term (env-with-var-related-to-parameters Env_4 Ty_Y <= (Ty_A Ty_B)))))

   (redex-let*
    formality-ty

    (; Ty_extruded <= X, but in U1 (same as A, B)
     ;
     ; Wind up with
     ;
     ; X <= Ty_extruded <= A, B
     ((Env_6 Ty_xe Goals) (term (extrude-parameter Env_5 Universe_1 <= Ty_X)))
     ((() (Ty_ye)) (term (variable-bounds Env_6 Ty_xe)))
     )

    (test-equal
     (term (universe-of-var-in-env Env_6 Ty_xe))
     (term Universe_1))

    (test-equal
     (term (universe-of-var-in-env Env_6 Ty_ye))
     (term Universe_1))

    (test-equal
     (term (variable-bounds Env_6 Ty_ye))
     (term (() (Ty_A Ty_B))))

    (test-equal
     (term Goals)
     (term ()))

    )

   )
  )