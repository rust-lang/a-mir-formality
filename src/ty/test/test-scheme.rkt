#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../scheme.rkt"
         "../inequalities.rkt"
         "../user-ty.rkt"
         "../../logic/substitution.rkt"
         "../../logic/instantiate.rkt"
         )

(module+ test
  (redex-let*
   formality-ty
   [((Env_1 () (Ty_T)) (term (instantiate-quantified EmptyEnv (∀ ((type T)) ()))))
    ((Env_2 () (Ty_A Ty_B)) (term (instantiate-quantified Env_1 (∃ ((type A) (type B)) ()))))
    ((Env_3 () (Lt_I Lt_J Lt_K Lt_L)) (term (instantiate-quantified Env_2 (∃ ((lifetime I) (lifetime J) (lifetime K) (lifetime L)) ()))))
    (Env_4 (term (env-with-var-mapped-to Env_3 Ty_A (rigid-ty (ref ()) (Lt_I (user-ty i32))))))
    (Env_5 (term (env-with-var-mapped-to Env_4 Ty_B (rigid-ty (ref ()) (Lt_J (user-ty i32))))))
    (Env_6 (term (env-with-var-related-to-parameter Env_5 Lt_I <= Lt_K)))
    (Env_7 (term (env-with-var-related-to-parameter Env_6 Lt_J <= Lt_K)))
    (Env_8 (term (env-with-var-related-to-parameter Env_7 Lt_K <= Lt_L)))
    (Env (term Env_8))
    ]

   (test-equal
    (term (extract-scheme Env (Ty_A Ty_B)))
    (term (∃
           ((lifetime Lt_J)
            (lifetime Lt_I)
            (lifetime Lt_K)
            (lifetime Lt_L))
           (implies
            ((Lt_J <= Lt_K) (Lt_I <= Lt_K) (Lt_K <= Lt_L))
            ((rigid-ty (ref ()) (Lt_I (rigid-ty i32 ())))
             (rigid-ty (ref ()) (Lt_J (rigid-ty i32 ())))))))
    )
   )
  )