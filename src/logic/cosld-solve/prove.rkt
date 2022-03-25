#lang racket
(require redex
         "../grammar.rkt"
         "../hook.rkt"
         "../substitution.rkt"
         "../instantiate.rkt"
         "../env.rkt"
         "../../util.rkt"
         "elaborate.rkt"
         "filter.rkt"
         "util.rkt")

(provide prove-top-level-goal/cosld)

(define-judgment-form formality-logic
  ;; Prove a "top-level" goal is true in the given environment
  ;; using the cosld solver. cosld is a basic [SLD] solving algorithm,
  ;; enriched to handle [FOHH] predicates as well as to
  ;; support a simple form of coinduction.
  ;;
  ;; [SLD]: https://en.wikipedia.org/wiki/SLD_resolution
  ;; [FOHH]: https://en.wikipedia.org/wiki/Harrop_formula
  #:mode (prove-top-level-goal/cosld I I O)
  #:contract (prove-top-level-goal/cosld Env Goal Env)

  [(prove Env (() ()) Goal Env_out)
   ---------------
   (prove-top-level-goal/cosld Env Goal Env_out)
   ]
  )

(define-judgment-form formality-logic
  ;; Convenience judgment that extracts the Substitution from the Env for testing.
  #:mode (prove-top-level-goal-substitution I I O)
  #:contract (prove-top-level-goal-substitution Env Goal Substitution)

  [(prove Env (() ()) Goal Env_out)
   (where/error Substitution (env-substitution Env_out))
   ---------------
   (prove-top-level-goal-substitution Env Goal Substitution)
   ]
  )

(define-judgment-form formality-logic
  #:mode (prove I I I O)
  #:contract (prove Env Prove/Stacks Goal Env_out)

  [(where #t (is-predicate-goal? Predicate))
   (not-in-stacks Env Predicate Prove/Stacks)
   (where (_ ... Clause _ ... ) (filter-clauses Env (env-clauses-for-predicate Env Predicate) Predicate))
   (clause-proves Env Prove/Stacks + Clause Predicate Env_out)
   --------------- "prove-clause"
   (prove Env Prove/Stacks Predicate Env_out)
   ]

  [(where #t (is-predicate-goal? Predicate))
   (not-in-stacks Env Predicate Prove/Stacks)
   (where (_ ... Hypothesis _ ... ) (filter-clauses Env (env-hypotheses (elaborate-hypotheses Env)) Predicate))
   (clause-proves Env Prove/Stacks - Hypothesis Predicate Env_out)
   --------------- "prove-hypotheses-imply"
   (prove Env Prove/Stacks Predicate Env_out)
   ]

  [(where #t (is-predicate-goal? Predicate))
   (where #t (in? Predicate Predicates_co))
   --------------- "prove-coinductive-cycle"
   (prove Env (_ Predicates_co) Predicate Env)
   ]

  [(where Env_out (relate-parameters Env Relation))
   --------------- "prove-relate"
   (prove Env Prove/Stacks Relation Env_out)
   ]

  [(prove-all Env Prove/Stacks Goals Env_out)
   --------------- "prove-all"
   (prove Env Prove/Stacks (All Goals) Env_out)
   ]

  [(prove Env Prove/Stacks Goal_1 Env_out)
   --------------- "prove-any"
   (prove Env Prove/Stacks (Any (Goal_0 ... Goal_1 Goal_2 ...)) Env_out)
   ]

  [(where Env_1 (env-with-hypotheses Env Hypotheses))
   (prove Env_1 Prove/Stacks Goal Env_out)
   --------------- "prove-implies"
   (prove Env Prove/Stacks (Implies Hypotheses Goal) (reset Env () Env_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (ForAll KindedVarIds Goal)))
   (prove Env_1 Prove/Stacks Goal_1 Env_out)
   --------------- "prove-forall"
   (prove Env Prove/Stacks (ForAll KindedVarIds Goal) (reset Env VarIds_new Env_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (Exists KindedVarIds Goal)))
   (prove Env_1 Prove/Stacks Goal_1 Env_out)
   --------------- "prove-exists"
   (prove Env Prove/Stacks (Exists KindedVarIds Goal) (reset Env VarIds_new Env_out))
   ]

  )

(define-judgment-form formality-logic
  #:mode (not-in-stacks I I I)
  #:contract (not-in-stacks Env Predicate Prove/Stacks)

  [(where #f (in? Predicate Predicates_i))
   (where #f (in? Predicate Predicates_c))
   --------------- "prove-exists"
   (not-in-stacks Env Predicate (Predicates_i Predicates_c))
   ]
  )

(define-judgment-form formality-logic
  #:mode (prove-all I I I O)
  #:contract (prove-all Env Prove/Stacks Goals Env_out)

  [----------------
   (prove-all Env Prove/Stacks () Env)]

  [(prove Env Prove/Stacks Goal_0 Env_1)
   (where/error (Goals_subst Predicates_subst) (apply-substitution-from-env Env_1 ((Goal_1 ...) Prove/Stacks)))
   (prove-all Env_1 Predicates_subst Goals_subst Env_out)
   ----------------
   (prove-all Env Prove/Stacks (Goal_0 Goal_1 ...) Env_out)]

  )

(define-judgment-form formality-logic
  #:mode (clause-proves I I I I I O)
  #:contract (clause-proves Env Prove/Stacks Prove/Coinductive Clause Predicate Env_out)

  [(where #t (is-predicate-goal? Predicate_1))
   (where Env_out (equate-predicates Env Predicate_1 Predicate_2))
   --------------- "clause-fact"
   (clause-proves Env Prove/Stacks Prove/Coinductive Predicate_1 Predicate_2 Env_out)
   ]

  [(where Env_eq (equate-predicates Env Predicate_1 Predicate_2))
   (where/error Prove/Stacks_pushed (push-on-stack Prove/Stacks Prove/Coinductive Predicate_2))
   (where/error (Goals_subst Prove/Stacks_subst) (apply-substitution-from-env Env_eq (Goals Prove/Stacks_pushed)))
   (prove-all Env Prove/Stacks_subst Goals_subst Env_out)
   --------------- "clause-backchain"
   (clause-proves Env Prove/Stacks Prove/Coinductive (Implies Goals Predicate_1) Predicate_2 Env_out)
   ]

  [(where/error (Env_i Clause_i VarIds_i) (instantiate-quantified Env (Exists KindedVarIds Clause)))
   (clause-proves Env_i Prove/Stacks Prove/Coinductive Clause_i Predicate Env_out)
   --------------- "clause-forall"
   (clause-proves Env Prove/Stacks Prove/Coinductive (ForAll KindedVarIds Clause) Predicate (reset Env VarIds_i Env_out))
   ]

  )

(define-metafunction formality-logic
  push-on-stack : Prove/Stacks Prove/Coinductive Predicate  -> Prove/Stacks

  [(push-on-stack (Predicates_i (Predicate_c ...)) + Predicate)
   (Predicates_i (Predicate Predicate_c ...))
   ]

  [(push-on-stack ((Predicate_i ...) (Predicate_c ...)) - Predicate)
   ((Predicate Predicate_c ... Predicate_i ...) ())
   ]
  )


(module+ test
  (require "../test/hook.rkt")

  (redex-let*
   formality-logic
   ((; A is in U0
     (Env_0 Term_A (VarId_0)) (term (instantiate-quantified EmptyEnv (Exists ((TyKind A)) A))))
    (; V is a placeholder in U1
     (Env_1 Term_T (VarId_1)) (term (instantiate-quantified Env_0 (ForAll ((TyKind T)) T))))
    (; X is in U1
     (Env_2 Term_X (VarId_2)) (term (instantiate-quantified Env_1 (Exists ((TyKind X)) X))))
    (; Y, Z are in U1
     (Env_3 (Term_Y Term_Z) VarIds_3) (term (instantiate-quantified Env_2 (Exists ((TyKind Y) (TyKind Z)) (Y Z)))))
    (Env_4 (term (relate-parameters Env_3 ((TyRigid Vec (Term_A)) == (TyRigid Vec (Term_X))))))
    (Env_5 (term (reset Env_2 VarIds_3 Env_4)))
    )

   ;; check that X was originally in U1 but was reduced to U0,
   ;; and that reduction survived the "reset-env" call
   (test-equal (term (universe-of-var-in-env Env_2 Term_X)) (term (next-universe RootUniverse)))
   (test-equal (term (universe-of-var-in-env Env_4 Term_X)) (term RootUniverse))
   (test-equal (term (universe-of-var-in-env Env_5 Term_X)) (term RootUniverse))

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal/cosld EmptyEnv (All ()) Env_out) Env_out)
            (term (EmptyEnv))))

   (redex-let*
    formality-logic
    ((Env (term (env-with-vars-in-current-universe EmptyEnv Exists (T U V)))))
    (test-equal
     (judgment-holds (prove-top-level-goal-substitution
                      Env
                      (All ((T == (TyRigid Vec (U)))
                            (U == (TyRigid Vec (V)))
                            (V == (TyRigid i32 ()))))
                      Substitution_out)
                     Substitution_out)
     (term (((V (TyRigid i32 ()))
             (U (TyRigid Vec ((TyRigid i32 ()))))
             (T (TyRigid Vec ((TyRigid Vec ((TyRigid i32 ()))))))
             ))))
    )

   (test-equal
    (judgment-holds (prove-top-level-goal/cosld
                     EmptyEnv
                     (ForAll ((TyKind T))
                             (Implies ((Implemented (Debug (T))))
                                      (Implemented (Debug (T)))))
                     Env_out)
                    Env_out)
    (term (EmptyEnv)))

   (test-equal
    (judgment-holds (prove-top-level-goal/cosld
                     (env-with-vars-in-current-universe EmptyEnv Exists (X))
                     (ForAll ((TyKind T))
                             (T == X))
                     Env_out)
                    Env_out)
    (term ()))

   (test-equal
    (judgment-holds (prove-top-level-goal/cosld
                     EmptyEnv
                     (ForAll ((TyKind T))
                             (Exists ((TyKind X))
                                     (T == X)))
                     Env_out)
                    Env_out)
    (term (EmptyEnv)))

   (redex-let*
    formality-logic
    ((Invariant_PartialEq-if-Eq (term (ForAll ((TyKind T)) (Implies ((Implemented (Eq (T))))
                                                                    (Implemented (PartialEq (T)))))))
     (Env (term (env-with-clauses-and-invariants ()
                                                 (Invariant_PartialEq-if-Eq)
                                                 ))))

    (test-equal
     (judgment-holds (prove-top-level-goal/cosld Env (Implemented (PartialEq ((TyRigid u32 ())))) Env_out)
                     Env_out)
     (term ()))

    (test-equal
     (judgment-holds (prove-top-level-goal/cosld
                      Env
                      (ForAll ((TyKind T)) (Implies ((Implemented (Eq (T))))
                                                    (Implemented (PartialEq (T)))))
                      Env_out)
                     Env_out)
     (term (Env)))
    )
   )

  ; Test for tricky case of cycle handling
  ;
  ; Clause: (p :- q)
  ; Invariant: (p => q)
  ; Hypothesis: (p => p)
  ;
  ; When trying to prove `Q`...
  ; * We can elaborate the hypothesis to `P => P` and `P => Q`.
  ; * Then we can try to prove `P`...
  ; * Which requires proving `P`...
  ; * ...which succeeds! Uh oh.
  ;
  ; Except it doesn't, because cycles that involve hypotheses are inductive
  ; and thus rejected.
  (redex-let*
   formality-logic
   ((Clause (term (Implies (q) p)))
    (Invariant (term (ForAll () (Implies (p) q))))
    (Hypothesis (term (Implies (p) p)))
    (Env (term (env-with-clauses-and-invariants (Clause)
                                                (Invariant)
                                                )))
    )

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal/cosld Env (Implies (Hypothesis) q) Env_out)
                            Env_out)
            (term ())))
   )

  ; Version of the above where our hypothesis is just `p`; this should be provable.
  (redex-let*
   formality-logic
   ((Clause (term (Implies (q) p)))
    (Invariant (term (ForAll () (Implies (p) q))))
    (Env (term (env-with-clauses-and-invariants (Clause)
                                                (Invariant)
                                                )))
    )

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal/cosld Env (Implies (p) q) Env_out)
                            Env_out)
            (term (Env))))
   )

  )
