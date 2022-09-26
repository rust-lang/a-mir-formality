#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "hook.rkt"
         "substitution.rkt"
         "instantiate.rkt"
         "env.rkt"
         "match.rkt"
         "../util.rkt"
         )
(provide elaborate-hypotheses)

; (define-metafunction formality-logic
;   ;; Given an Env with some set of hypotheses H, returns a new environment
;   ;; that adds in any "implied bounds", i.e., the hypotheses implied by the
;   ;; program Invariants.
;   hypothesized-placeholder-bounds : Env VarId -> Env
;   #:pre (env-contains-placeholder-var Env VarId)

;   [(hypothesized-placeholder-bounds Env VarId)
;    (where/error Env_1 (elaborate-hypotheses Env)
;                 (where/))
;    ]
;   )

; (define-judgment-form formality-logic
;   ;; Helper judgment, elaborates a hypothesis in all the ways possible.
;   #:mode (hypothesized-placeholder-bound I I O)
;   #:contract (hypothesized-placeholder-bound Env VarId Parameter_out)

;   [(where (_ ... Hypothesis _ ...) (env-hypotheses (elaborate-hypotheses Env)))
;    (hypothesis-implies-placeholder-bound Env VarId Hypothesis Parameter_out)
;    ----------
;    (hypothesized-placeholder-bound Env VarId Parameter_out)
;    ]

;   )

; (define-judgment-form formality-logic
;   ;; Helper judgment, elaborates a hypothesis in all the ways possible.
;   #:mode (hypothesis-implies-placeholder-bound I I I O)
;   #:contract (hypothesis-implies-placeholder-bound Env VarId Hypothesis Parameter_out)

;   [(where (_ ... Hypothesis _ ...) (env-hypotheses (elaborate-hypotheses Env)))
;    ( Env VarId Hypothesis Parameter_out)
;    ----------
;    (hypothesis-implies-placeholder-bound Env VarId  Parameter_out)
;    ]

;   )

(define-metafunction formality-logic
  ;; Given an Env with some set of hypotheses H, returns a new environment
  ;; that adds in any "implied bounds", i.e., the hypotheses implied by the
  ;; program Invariants.
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

(define-metafunction formality-logic
  ;; Helper function: elaborates the given list of hypotheses, adding the results
  ;; to Env.
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

(define-metafunction formality-logic
  ;; Helper function: elaborates a single hypothesis, adding the results
  ;; to Env.
  elaborate-hypothesis-one-step : Env Hypothesis -> Hypotheses

  ([elaborate-hypothesis-one-step Env Hypothesis_in]
   ,(judgment-holds (hypothesis-elaborates-one-step Env Hypothesis_in Hypothesis_out) Hypothesis_out)
   )
  )

(define-judgment-form formality-logic
  ;; Helper judgment, elaborates a hypothesis in all the ways possible.
  #:mode (hypothesis-elaborates-one-step I I O)
  #:contract (hypothesis-elaborates-one-step Env Hypothesis Hypothesis)

  [(where #t (is-predicate-hypothesis? Env Predicate_in))
   (where (_ ... Invariant _ ...) (env-invariants Env))

   ; the Invariant will have the form `∀ (Vars...) Term_bound`:
   ; create a version of `Term_bound` where `Vars` are fresh identifiers
   ; that don't appear in the environment
   (where/error (∀ ((ParameterKind VarId) ...) Term_bound) Invariant)
   (where/error (VarId_fresh ...) (fresh-var-ids Env (VarId ...)))
   (where/error Substitution_freshen ((VarId VarId_fresh) ...))
   (where/error Term_bound-fresh (apply-substitution Substitution_freshen Term_bound))

   ; that bound term must have form `P => Q`, try to find some assignment
   ; for the (fresh) `Vars` that matches `P` against `Predicate_in`
   (where/error (implies (Predicate_condition) AtomicGoal_consequence) Term_bound-fresh)
   (where Substitution_m (match-terms (VarId_fresh ...) Predicate_condition Predicate_in))

   ; if successful, we can add `Q` to the result (after substituting the result of the match)
   (where/error AtomicGoal_out (apply-substitution Substitution_m AtomicGoal_consequence))
   ------------------- "elaborate-predicate"
   (hypothesis-elaborates-one-step Env Predicate_in AtomicGoal_out)
   ]

  [(where #t (is-relation-goal? Env Relation))
   (where [_ ... AtomicGoal _ ...] (elaborate-relation Env Relation))
   -------------------- "elaborate-relation"
   (hypothesis-elaborates-one-step Env Relation AtomicGoal)
   ]

  [(hypothesis-elaborates-one-step Env
                                   Hypothesis_in
                                   Hypothesis_out)
   ------------------- "elaborate-forall"
   (hypothesis-elaborates-one-step Env
                                   (∀ KindedVarIds Hypothesis_in)
                                   (∀ KindedVarIds Hypothesis_out))
   ]

  [(hypothesis-elaborates-one-step Env
                                   AtomicGoal_in
                                   AtomicGoal_out)
   (where #t (is-atomic-goal? Env AtomicGoal_out))
   ------------------- "elaborate-implies-map"
   (hypothesis-elaborates-one-step Env
                                   (implies (Goal_in ...) AtomicGoal_in)
                                   (implies (Goal_in ...) AtomicGoal_out))
   ]

  [(hypothesis-elaborates-one-step Env
                                   AtomicGoal_in
                                   (implies (Goal_out ...) AtomicGoal_out))
   ------------------- "elaborate-implies-flatten"
   (hypothesis-elaborates-one-step Env
                                   (implies (Goal_in ...) AtomicGoal_in)
                                   (implies (Goal_in ... Goal_out ...) AtomicGoal_out))
   ]
  )

(module+ test
  (require "test/hook.rkt")

  (redex-let*
   formality-logic
   [(Env (term (env-with-clauses-and-invariants ()
                                                ((∀ ((type T)) (implies ((is-implemented (Eq (T)))) (is-implemented (PartialEq (T)))))
                                                 (∀ ((type T)) (implies ((is-implemented (Ord (T)))) (is-implemented (PartialOrd (T)))))
                                                 (∀ ((type T)) (implies ((is-implemented (Ord (T)))) (is-implemented (Eq (T)))))
                                                 (∀ ((type T)) (implies ((is-implemented (PartialOrd (T)))) (is-implemented (PartialEq (T)))))
                                                 ))))
    ]

   (traced '()
           (test-equal
            (term (elaborate-hypothesis-one-step Env (is-implemented (Ord ((user-ty u32))))))
            (term ((is-implemented (Eq ((user-ty u32))))
                   (is-implemented (PartialOrd ((user-ty u32))))))))

   (traced '()
           (test-equal
            (term (env-hypotheses (elaborate-hypotheses (env-with-hypotheses Env ((is-implemented (Ord ((user-ty u32)))))))))
            (term ((is-implemented (Ord ((user-ty u32))))
                   (is-implemented (Eq ((user-ty u32))))
                   (is-implemented (PartialOrd ((user-ty u32))))
                   (is-implemented (PartialEq ((user-ty u32))))))))

   (traced '()
           (test-alpha-equivalent
            formality-logic
            (term (env-hypotheses (elaborate-hypotheses
                                   (env-with-hypotheses Env
                                                        ((∀ ((type T)) (is-implemented (Ord (T)))))))))
            (term ((∀ ((type T)) (is-implemented (Ord (T))))
                   (∀ ((type T)) (is-implemented (Eq (T))))
                   (∀ ((type T)) (is-implemented (PartialOrd (T))))
                   (∀ ((type T)) (is-implemented (PartialEq (T)))))))

           )
   )
  )