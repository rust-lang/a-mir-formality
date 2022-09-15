#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../logic/substitution.rkt"
         "../../logic/instantiate.rkt"
         "../grammar.rkt"
         "../solution.rkt"
         "../inequalities.rkt"
         "../user-ty.rkt"
         "hook.rkt"
         )

(module+ test
  (redex-let*
   formality-ty
   [(Env_0 (term (env-with-clauses-invariants-and-generics [] [] [])))

    ; Model a canonical query (?A ?B) that has an answer ?A = Vec<?C>, ?C <: ?B

    ; Create placeholder T and inference variables ?A and ?B
    ; that are meant to be part of the "canonical query"
    ((Env_1 () (Ty_T)) (term (instantiate-quantified EmptyEnv (∀ ((type T)) ()))))
    ((Env_2 () (Ty_A Ty_B)) (term (instantiate-quantified Env_1 (∃ ((type A) (type B)) ()))))

    ; Create the "solution" constraints:
    ; * there exists a C...
    ((Env_3 () (Ty_C)) (term (instantiate-quantified Env_2 (∃ ((type C)) ()))))
    ; * where C <: B
    (Env_4 (term (env-with-var-related-to-parameter Env_3 Ty_C <= Ty_B)))
    ; * and A == Vec<B>
    (Env_5 (term (env-with-var-mapped-to Env_4 Ty_A (user-ty (Vec < Ty_C >)))))
    (Env_N (term Env_5))
    ]

   (test-match-terms
    formality-ty
    (extract-solution Env_N (Ty_A Ty_B))
    (((VarId_C type ∃ (universe 1)))
     (((Ty_A (rigid-ty Vec (VarId_C))))
      ((VarId_C <= Ty_B)))))
   )

  )
