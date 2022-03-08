#lang racket
(require redex
         "../grammar.rkt"
         "../substitution.rkt"
         "../instantiate.rkt"
         "../unify.rkt"
         "../../util.rkt"
         "hypothesize.rkt"
         "util.rkt")

(provide prove prove-top-level-goal)

(define-judgment-form formality-ty
  ;; Prove a "top-level" goal is true in the given environment
  ;; using the cosld solver. cosld is a basic [SLD] solving algorithm,
  ;; enriched to handle [FOHH] predicates as well as to
  ;; support a simple form of coinduction.
  ;;
  ;; [SLD]: https://en.wikipedia.org/wiki/SLD_resolution
  ;; [FOHH]: https://en.wikipedia.org/wiki/Harrop_formula
  #:mode (prove-top-level-goal I I O)
  #:contract (prove-top-level-goal Env Goal EnvSubstitution)

  [(prove Env () Goal EnvSubstitution_out)
   ---------------
   (prove-top-level-goal Env Goal EnvSubstitution_out)
   ]
  )

(define-judgment-form formality-ty
  #:mode (prove I I I O)
  #:contract (prove Env Predicates_stack Goal EnvSubstitution)

  [(where #f (in? Predicate Predicates_stack))
   (where (_ ... Clause _ ... ) (flatten ((env-clauses Env) (env-hypotheses Env))))
   (Clause-proves Env Predicates_stack Clause Predicate EnvSubstitution_out)
   --------------- "prove-clause"
   (prove Env Predicates_stack Predicate EnvSubstitution_out)
   ]

  [(where #f (in? Predicate Predicates_stack))
   (Hypotheses-imply Env () Predicate EnvSubstitution_out)
   --------------- "prove-hypotheses-imply"
   (prove Env Predicates_stack Predicate EnvSubstitution_out)
   ]

  [(where #t (in? Predicate Predicates_stack))
   --------------- "prove-cycle"
   (prove Env Predicates_stack Predicate (Env ()))
   ]

  [(equate Env Term_1 Term_2 EnvSubstitution_out)
   --------------- "prove-equate"
   (prove Env Predicates_stack (Equate Term_1 Term_2) EnvSubstitution_out)
   ]

  [(prove-all Env Predicates_stack Goals EnvSubstitution_out)
   --------------- "prove-all"
   (prove Env Predicates_stack (All Goals) EnvSubstitution_out)
   ]

  [(prove Env Predicates_stack Goal_1 EnvSubstitution_out)
   --------------- "prove-any"
   (prove Env Predicates_stack (Any (Goal_0 ... Goal_1 Goal_2 ...)) EnvSubstitution_out)
   ]

  [(where Env_1 (env-with-hypotheses Env Hypotheses))
   (prove Env_1 Predicates_stack Goal EnvSubstitution_out)
   --------------- "prove-implies"
   (prove Env Predicates_stack (Implies Hypotheses Goal) (reset Env () EnvSubstitution_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env ForAll KindedVarIds Goal))
   (prove Env_1 Predicates_stack Goal_1 EnvSubstitution_out)
   --------------- "prove-forall"
   (prove Env Predicates_stack (ForAll KindedVarIds Goal) (reset Env VarIds_new EnvSubstitution_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env Exists KindedVarIds Goal))
   (prove Env_1 Predicates_stack Goal_1 EnvSubstitution_out)
   --------------- "prove-exists"
   (prove Env Predicates_stack (Exists KindedVarIds Goal) (reset Env VarIds_new EnvSubstitution_out))
   ]

  )

(define-judgment-form formality-ty
  #:mode (prove-all I I I O)
  #:contract (prove-all Env Predicates_stack Goals EnvSubstitution)

  [----------------
   (prove-all Env Predicates_stack () (Env ()))]

  [(prove Env Predicates_stack Goal_0 (Env_1 Substitution_1))
   (where/error (Goals_subst Predicates_subst) (apply-substitution Substitution_1 ((Goal_1 ...) Predicates_stack)))
   (prove-all Env_1 Predicates_subst Goals_subst EnvSubstitution_out)
   ----------------
   (prove-all Env Predicates_stack (Goal_0 Goal_1 ...) (merge-substitution Substitution_1 EnvSubstitution_out))]

  )

(define-judgment-form formality-ty
  #:mode (Clause-proves I I I I O)
  #:contract (Clause-proves Env Predicates_stack Clause Predicate EnvSubstitution)

  [(equate Env Predicate_1 Predicate_2 EnvSubstitution)
   --------------- "clause-fact"
   (Clause-proves Env Predicates_stack Predicate_1 Predicate_2 EnvSubstitution)
   ]

  [(equate Env Predicate_1 Predicate_2 (Env_eq Substitution_eq))
   (where/error (Goals_subst Predicates_subst)
                (apply-substitution Substitution_eq (Goals (Predicate_2 Predicate_stack ...))))
   (prove-all Env Predicates_subst Goals_subst EnvSubstitution)
   --------------- "clause-backchain"
   (Clause-proves Env (Predicate_stack ...) (Implies Goals Predicate_1) Predicate_2 EnvSubstitution)
   ]

  [(where/error (Env_i Clause_i VarIds_i) (instantiate-quantified Env Exists KindedVarIds Clause))
   (Clause-proves Env_i Predicates_stack Clause_i Predicate EnvSubstitution)
   --------------- "clause-forall"
   (Clause-proves Env Predicates_stack (ForAll KindedVarIds Clause) Predicate (reset Env VarIds_i EnvSubstitution))
   ]

  )


(module+ test
  (redex-let*
   formality-ty
   ((; A is in U0
     (Env_0 Ty_A (VarId_0)) (term (instantiate-quantified EmptyEnv Exists ((TyKind A)) A)))
    (; V is a placeholder in U1
     (Env_1 Ty_T (VarId_1)) (term (instantiate-quantified Env_0 ForAll ((TyKind T)) T)))
    (; X is in U1
     (Env_2 Ty_X (VarId_2)) (term (instantiate-quantified Env_1 Exists ((TyKind X)) X)))
    (; Y, Z are in U1
     (Env_3 (Ty_Y Ty_Z) VarIds_3) (term (instantiate-quantified Env_2 Exists ((TyKind Y) (TyKind Z)) (Y Z))))
    ((Env_4 Substitution_out) (term (most-general-unifier Env_3 (((TyApply Vec (Ty_A)) (TyApply Vec (Ty_X)))))))
    (Env_5 (term (copy-universes Env_2 VarIds_3 Env_4)))
    )

   ;; check that X was originally in U1 but was reduced to U0,
   ;; and that reduction survived the "reset-env" call
   (test-equal (term (universe-of-var-in-env Env_2 Ty_X)) (term (next-universe RootUniverse)))
   (test-equal (term (universe-of-var-in-env Env_4 Ty_X)) (term RootUniverse))
   (test-equal (term (universe-of-var-in-env Env_5 Ty_X)) (term RootUniverse))

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal EmptyEnv (All ()) EnvSubstitution) EnvSubstitution)
            (term ((EmptyEnv ())))))

   (redex-let*
    formality-ty
    ((Env (term (env-with-vars-in-current-universe EmptyEnv Exists (T U V)))))
    (test-equal
     (judgment-holds (prove-top-level-goal
                      Env
                      (All ((Equate T (TyApply Vec (U)))
                            (Equate U (TyApply Vec (V)))
                            (Equate V (TyApply i32 ()))))
                      EnvSubstitution)
                     EnvSubstitution)
     (term ((Env ((T (TyApply Vec ((TyApply Vec ((TyApply i32 ()))))))
                  (U (TyApply Vec ((TyApply i32 ()))))
                  (V (TyApply i32 ()))))))))

   (test-equal
    (judgment-holds (prove-top-level-goal
                     EmptyEnv
                     (ForAll ((TyKind T))
                             (Implies ((Implemented (Debug (T))))
                                      (Implemented (Debug (T)))))
                     EnvSubstitution)
                    EnvSubstitution)
    (term ((EmptyEnv ()))))

   (test-equal
    (judgment-holds (prove-top-level-goal
                     (env-with-vars-in-current-universe EmptyEnv Exists (X))
                     (ForAll ((TyKind T))
                             (Equate T X))
                     EnvSubstitution)
                    EnvSubstitution)
    (term ()))

   (test-equal
    (judgment-holds (prove-top-level-goal
                     EmptyEnv
                     (ForAll ((TyKind T))
                             (Exists ((TyKind X))
                                     (Equate T X)))
                     EnvSubstitution)
                    EnvSubstitution)
    (term ((EmptyEnv ()))))

   (redex-let*
    formality-ty
    ((Invariant_PartialEq-if-Eq (term (ForAll ((TyKind T)) (Implies ((Implemented (Eq (T))))
                                                                    (Implemented (PartialEq (T)))))))
     (Env (term (env-with-clauses-and-invariants EmptyEnv
                                                 ()
                                                 (Invariant_PartialEq-if-Eq)
                                                 ))))

    (test-equal
     (judgment-holds (prove-top-level-goal Env (Implemented (PartialEq ((TyApply u32 ())))) EnvSubstitution)
                     EnvSubstitution)
     (term ()))

    (test-equal
     (judgment-holds (prove-top-level-goal
                      Env
                      (ForAll ((TyKind T)) (Implies ((Implemented (Eq (T))))
                                                    (Implemented (PartialEq (T)))))
                      EnvSubstitution)
                     EnvSubstitution)
     (term ((Env ()))))
    )
   ))
