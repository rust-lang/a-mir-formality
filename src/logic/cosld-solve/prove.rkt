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

  [(prove Env () Goal Env_out)
   ---------------
   (prove-top-level-goal/cosld Env Goal Env_out)
   ]
  )

(define-judgment-form formality-logic
  ;; Convenience judgment that extracts the Substitution from the Env for testing.
  #:mode (prove-top-level-goal-substitution I I O)
  #:contract (prove-top-level-goal-substitution Env Goal Substitution)

  [(prove Env () Goal Env_out)
   (where/error Substitution (env-substitution Env_out))
   ---------------
   (prove-top-level-goal-substitution Env Goal Substitution)
   ]
  )

(define-judgment-form formality-logic
  #:mode (prove I I I O)
  #:contract (prove Env Predicates_stack Goal Env_out)

  [(where #t (is-predicate-goal? Predicate))
   (where #f (in? Predicate Predicates_stack))
   (where (_ ... Clause _ ... ) (filter-clauses Env (env-clauses-for-predicate Env Predicate) Predicate))
   (clause-proves Env Predicates_stack Clause Predicate Env_out)
   --------------- "prove-clause"
   (prove Env Predicates_stack Predicate Env_out)
   ]

  [(where #t (is-predicate-goal? Predicate))
   (where #f (in? Predicate Predicates_stack))
   (where (_ ... Hypothesis _ ... ) (filter-clauses Env (env-hypotheses (elaborate-hypotheses Env)) Predicate))
   (clause-proves Env Predicates_stack Hypothesis Predicate Env_out)
   --------------- "prove-hypotheses-imply"
   (prove Env Predicates_stack Predicate Env_out)
   ]

  [(where #t (is-predicate-goal? Predicate))
   (where #t (in? Predicate Predicates_stack))
   --------------- "prove-cycle"
   (prove Env Predicates_stack Predicate Env)
   ]

  [(where Env_out (relate-parameters Env Relation))
   --------------- "prove-relate"
   (prove Env Predicates_stack Relation Env_out)
   ]

  [(prove-all Env Predicates_stack Goals Env_out)
   --------------- "prove-all"
   (prove Env Predicates_stack (All Goals) Env_out)
   ]

  [(prove Env Predicates_stack Goal_1 Env_out)
   --------------- "prove-any"
   (prove Env Predicates_stack (Any (Goal_0 ... Goal_1 Goal_2 ...)) Env_out)
   ]

  [(where Env_1 (env-with-hypotheses Env Hypotheses))
   (prove Env_1 Predicates_stack Goal Env_out)
   --------------- "prove-implies"
   (prove Env Predicates_stack (Implies Hypotheses Goal) (reset Env () Env_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (ForAll KindedVarIds Goal)))
   (prove Env_1 Predicates_stack Goal_1 Env_out)
   --------------- "prove-forall"
   (prove Env Predicates_stack (ForAll KindedVarIds Goal) (reset Env VarIds_new Env_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (Exists KindedVarIds Goal)))
   (prove Env_1 Predicates_stack Goal_1 Env_out)
   --------------- "prove-exists"
   (prove Env Predicates_stack (Exists KindedVarIds Goal) (reset Env VarIds_new Env_out))
   ]

  )

(define-judgment-form formality-logic
  #:mode (prove-all I I I O)
  #:contract (prove-all Env Predicates_stack Goals Env_out)

  [----------------
   (prove-all Env Predicates_stack () Env)]

  [(prove Env Predicates_stack Goal_0 Env_1)
   (where/error (Goals_subst Predicates_subst) (apply-substitution-from-env Env_1 ((Goal_1 ...) Predicates_stack)))
   (prove-all Env_1 Predicates_subst Goals_subst Env_out)
   ----------------
   (prove-all Env Predicates_stack (Goal_0 Goal_1 ...) Env_out)]

  )

(define-judgment-form formality-logic
  #:mode (clause-proves I I I I O)
  #:contract (clause-proves Env Predicates_stack Clause Predicate Env_out)

  [(where #t (is-predicate-goal? Predicate_1))
   (where Env_out (equate-predicates Env Predicate_1 Predicate_2))
   --------------- "clause-fact"
   (clause-proves Env Predicates_stack Predicate_1 Predicate_2 Env_out)
   ]

  [(where Env_eq (equate-predicates Env Predicate_1 Predicate_2))
   (where/error (Goals_subst Predicates_subst)
                (apply-substitution-from-env Env_eq (Goals (Predicate_2 Predicate_stack ...))))
   (prove-all Env Predicates_subst Goals_subst Env_out)
   --------------- "clause-backchain"
   (clause-proves Env (Predicate_stack ...) (Implies Goals Predicate_1) Predicate_2 Env_out)
   ]

  [(where/error (Env_i Clause_i VarIds_i) (instantiate-quantified Env (Exists KindedVarIds Clause)))
   (clause-proves Env_i Predicates_stack Clause_i Predicate Env_out)
   --------------- "clause-forall"
   (clause-proves Env Predicates_stack (ForAll KindedVarIds Clause) Predicate (reset Env VarIds_i Env_out))
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
   ))
