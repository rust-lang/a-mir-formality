#lang racket
(require redex
         "../grammar.rkt"
         "../substitution.rkt"
         "../instantiate.rkt"
         "../unify.rkt"
         "util.rkt"
         "filter.rkt"
         "../../util.rkt")
(provide hypothesis-elaborates-to)

(define-metafunction formality-ty
  elaborate-hypothesis-one-step : Env Hypothesis -> Hypotheses

  ([elaborate-hypothesis-one-step Env Hypothesis_in]
   ,(judgment-holds (hypothesis-elaborates-to Env Hypothesis_in Hypothesis_out) Hypothesis_out))
  )

(define-judgment-form formality-ty
  #:mode (hypothesis-elaborates-to I I O)
  #:contract (hypothesis-elaborates-to Env Hypothesis Hypothesis)

  [(where (_ ... Invariant _ ...) (env-invariants Env))
   (where/error (ForAll KindedVarIds Term) Invariant)
   (where (Env_1 (Implies (Predicate_condition) Predicate_consequence) VarIds)
          (instantiate-quantified Env (Exists KindedVarIds Term)))
   (where (Env_2 Substitution)
          (most-general-unifier/vars VarIds Env_1 ((Predicate_condition Predicate_in))))
   (where Predicate_out (apply-substitution Substitution Predicate_consequence))
   ------------------- "elaborate-predicate"
   (hypothesis-elaborates-to Env Predicate_in Predicate_out)
   ]

  [(hypothesis-elaborates-to Env
                             Hypothesis_in
                             Hypothesis_out)
   ------------------- "elaborate-forall"
   (hypothesis-elaborates-to Env
                             (ForAll KindedVarIds Hypothesis_in)
                             (ForAll KindedVarIds Hypothesis_out))
   ]

  [(hypothesis-elaborates-to Env
                             Predicate_in
                             Predicate_out)
   ------------------- "elaborate-implies"
   (hypothesis-elaborates-to Env
                             (Implies Goals Predicate_in)
                             (Implies Goals Predicate_out))
   ]
  )

(module+ test
  (redex-let*
   formality-ty
   [(Env (term (env-with-clauses-and-invariants EmptyEnv
                                                ()
                                                ((ForAll ((TyKind T)) (Implies ((Implemented (Eq (T)))) (Implemented (PartialEq (T)))))
                                                 (ForAll ((TyKind T)) (Implies ((Implemented (Ord (T)))) (Implemented (PartialOrd (T)))))
                                                 (ForAll ((TyKind T)) (Implies ((Implemented (Ord (T)))) (Implemented (Eq (T)))))
                                                 (ForAll ((TyKind T)) (Implies ((Implemented (PartialOrd (T)))) (Implemented (PartialEq (T)))))
                                                 ))))
    ]

   (traced '()
           (test-equal
            (term (elaborate-hypothesis-one-step Env (Implemented (Ord ((scalar-ty u32))))))
            (term ((Implemented (Eq ((TyApply u32 ()))))
                   (Implemented (PartialOrd ((TyApply u32 ()))))))))
   )

  )