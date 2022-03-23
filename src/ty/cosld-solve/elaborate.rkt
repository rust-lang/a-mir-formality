#lang racket
(require redex
         racket/set
         "../grammar.rkt"
         "../hook.rkt"
         "../substitution.rkt"
         "../instantiate.rkt"
         "../unify.rkt"
         "util.rkt"
         "filter.rkt"
         "../../util.rkt")
(provide elaborate-hypotheses hypothesis-elaborates)

(define-metafunction formality-ty
  elaborate-hypotheses : Env -> Env

  [; fixed point reached?
   (elaborate-hypotheses Env)
   Env

   (where/error Hypotheses (env-hypotheses Env))
   (where Env (elaborate-hypotheses-one-step Env Hypotheses))
   ]

  [; new stuff added -- recurse
   (elaborate-hypotheses Env)
   (elaborate-hypotheses Env_1)

   (where/error Hypotheses (env-hypotheses Env))
   (where Env_1 (elaborate-hypotheses-one-step Env Hypotheses))
   ]
  )

(define-metafunction formality-ty
  elaborate-hypotheses-one-step : Env Hypotheses_to-elaborate -> Env

  ((elaborate-hypotheses-one-step Env ())
   Env
   )

  ((elaborate-hypotheses-one-step Env (Hypothesis_first Hypothesis_rest ...))
   (elaborate-hypotheses-one-step Env_new (Hypothesis_rest ...))

   (where/error Hypotheses_new (elaborate-hypothesis-one-step Env Hypothesis_first))
   (where/error Env_new (env-with-hypotheses Env Hypotheses_new))
   )
  )

(define-metafunction formality-ty
  elaborate-hypothesis-one-step : Env Hypothesis -> Hypotheses

  ([elaborate-hypothesis-one-step Env Hypothesis_in]
   ,(judgment-holds (hypothesis-elaborates-one-step Env Hypothesis_in Hypothesis_out) Hypothesis_out))
  )

(define-judgment-form formality-ty
  #:mode (hypothesis-elaborates I I O)
  #:contract (hypothesis-elaborates Env Hypothesis Hypothesis)

  [(hypotheses-elaborate-stack Env (Hypothesis_1) Hypothesis_2)
   ------------------- "hypothesis-elaborates-step"
   (hypothesis-elaborates Env Hypothesis_1 Hypothesis_2)
   ]

  [------------------- "hypothesis-elaborates-self"
   (hypothesis-elaborates Env Hypothesis Hypothesis)
   ]

  )

(define-judgment-form formality-ty
  #:mode (hypotheses-elaborate-stack I I O)
  #:contract (hypotheses-elaborate-stack Env Hypotheses Hypothesis)

  [(hypothesis-elaborates-one-step Env Hypothesis_1 Hypothesis_2)
   ------------------- "hypothesis-elaborates-step"
   (hypotheses-elaborate-stack Env (Hypothesis_0 ... Hypothesis_1) Hypothesis_2)
   ]

  [(hypothesis-elaborates-one-step Env Hypothesis_1 Hypothesis_2)
   (where #f (hypothesis-on-stack (Hypothesis_0 ... Hypothesis_1) Hypothesis_2))
   (hypotheses-elaborate-stack Env (Hypothesis_0 ... Hypothesis_1 Hypothesis_2) Hypothesis_3)
   ------------------- "hypothesis-elaborates-transitive"
   (hypotheses-elaborate-stack Env (Hypothesis_0 ... Hypothesis_1) Hypothesis_3)
   ]
  )

(define-metafunction formality-ty
  hypothesis-on-stack : Hypotheses Hypothesis -> boolean

  [(hypothesis-on-stack (_ ... Hypothesis_0 _ ...) Hypothesis_1)
   #t
   (where #t ,(alpha-equivalent? (term Hypothesis_0) (term Hypothesis_1)))]

  [(hypothesis-on-stack (_ ... Hypothesis_0 _ ...) Hypothesis_1)
   #f]
  )

(define-judgment-form formality-ty
  #:mode (hypothesis-elaborates-one-step I I O)
  #:contract (hypothesis-elaborates-one-step Env Hypothesis Hypothesis)

  [(where (_ ... Invariant _ ...) (env-invariants Env))
   (where/error (ForAll KindedVarIds Term) Invariant)
   (where (Env_1 (Implies (Predicate_condition) Predicate_consequence) VarIds)
          (instantiate-quantified Env (Exists KindedVarIds Term)))
   (where (Env_2 Substitution)
          (most-general-unifier/vars VarIds Env_1 ((Predicate_condition Predicate_in))))
   (where Predicate_out (apply-substitution Substitution Predicate_consequence))
   ------------------- "elaborate-predicate"
   (hypothesis-elaborates-one-step Env Predicate_in Predicate_out)
   ]

  [(hypothesis-elaborates-one-step Env
                                   Hypothesis_in
                                   Hypothesis_out)
   ------------------- "elaborate-forall"
   (hypothesis-elaborates-one-step Env
                                   (ForAll KindedVarIds Hypothesis_in)
                                   (ForAll KindedVarIds Hypothesis_out))
   ]

  [(hypothesis-elaborates-one-step Env
                                   Predicate_in
                                   Predicate_out)
   ------------------- "elaborate-implies"
   (hypothesis-elaborates-one-step Env
                                   (Implies Goals Predicate_in)
                                   (Implies Goals Predicate_out))
   ]
  )

(module+ test
  (redex-let*
   formality-ty
   [(Env (term (env-with-clauses-and-invariants ()
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

   (traced '()
           (test-equal
            (term (env-hypotheses (elaborate-hypotheses (env-with-hypotheses Env ((Implemented (Ord ((scalar-ty u32)))))))))
            (term ((Implemented (Ord ((TyApply u32 ()))))
                   (Implemented (Eq ((TyApply u32 ()))))
                   (Implemented (PartialOrd ((TyApply u32 ()))))
                   (Implemented (PartialEq ((TyApply u32 ()))))))))

   (traced '()
           (test-alpha-equivalent
            formality-ty
            (term (env-hypotheses (elaborate-hypotheses
                                   (env-with-hypotheses Env
                                                        ((ForAll ((TyKind T)) (Implemented (Ord (T)))))))))
            (term ((ForAll ((TyKind T)) (Implemented (Ord (T))))
                   (ForAll ((TyKind T)) (Implemented (Eq (T))))
                   (ForAll ((TyKind T)) (Implemented (PartialOrd (T))))
                   (ForAll ((TyKind T)) (Implemented (PartialEq (T))))))))

   (traced '()
           (test-alpha-equivalent
            formality-ty
            (judgment-holds (hypothesis-elaborates Env (ForAll ((TyKind T)) (Implemented (Ord (T)))) Hypothesis_out) Hypothesis_out)
            (term ((ForAll ((TyKind T)) (Implemented (Ord (T))))
                   (ForAll ((TyKind T)) (Implemented (Eq (T))))
                   (ForAll
                    ((TyKind T))
                    (Implemented (PartialOrd (T))))
                   (ForAll
                    ((TyKind T))
                    (Implemented (PartialEq (T)))))))
           )
   )
  )