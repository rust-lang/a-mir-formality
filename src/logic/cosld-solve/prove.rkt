#lang racket
(require redex
         "../../util.rkt"
         "../grammar.rkt"
         "../hook.rkt"
         "../substitution.rkt"
         "../instantiate.rkt"
         "../env.rkt"
         "../elaborate.rkt"
         "filter.rkt"
         )

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

  [(where #t (is-predicate? Env Predicate))
   (not-in-stacks Env Predicate Prove/Stacks)
   (where (_ ... Clause _ ... ) (filter-clauses Env (env-clauses-for-predicate Env Predicate) Predicate))
   (clause-proves Env Prove/Stacks + Clause Predicate Env_out)
   --------------- "prove-clause"
   (prove Env Prove/Stacks Predicate Env_out)
   ]

  [(where #t (is-predicate? Env Predicate))
   (not-in-stacks Env Predicate Prove/Stacks)
   (where (_ ... Hypothesis _ ... ) (filter-clauses Env (env-hypotheses (elaborate-hypotheses Env)) Predicate))
   (clause-proves Env Prove/Stacks - Hypothesis Predicate Env_out)
   --------------- "prove-hypotheses-imply"
   (prove Env Prove/Stacks Predicate Env_out)
   ]

  [(where #t (is-predicate? Env Predicate))
   (where #t (in? (apply-substitution-from-env Env Predicate)
                  (apply-substitution-from-env Env Predicates_co)))
   --------------- "prove-coinductive-cycle"
   (prove Env (_ Predicates_co) Predicate Env)
   ]

  [(where #t (is-relation? Env Relation))
   (where (Env_eq Goals_eq) (relate-parameters Env Relation))
   (prove-all Env_eq Prove/Stacks Goals_eq Env_out)
   --------------- "prove-relate"
   (prove Env Prove/Stacks Relation Env_out)
   ]

  [(prove-all Env Prove/Stacks Goals Env_out)
   --------------- "prove-all"
   (prove Env Prove/Stacks (&& Goals) Env_out)
   ]

  [(prove Env Prove/Stacks Goal_1 Env_out)
   --------------- "prove-any"
   (prove Env Prove/Stacks (Any (Goal_0 ... Goal_1 Goal_2 ...)) Env_out)
   ]

  [(where Env_1 (env-with-hypotheses Env Hypotheses))
   (prove Env_1 Prove/Stacks Goal Env_out)
   --------------- "prove-implies"
   (prove Env Prove/Stacks (implies Hypotheses Goal) (reset Env () Env_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (∀ KindedVarIds Goal)))
   (prove Env_1 Prove/Stacks Goal_1 Env_out)
   --------------- "prove-forall"
   (prove Env Prove/Stacks (∀ KindedVarIds Goal) (reset Env VarIds_new Env_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (∃ KindedVarIds Goal)))
   (prove Env_1 Prove/Stacks Goal_1 Env_out)
   --------------- "prove-exists"
   (prove Env Prove/Stacks (∃ KindedVarIds Goal) (reset Env VarIds_new Env_out))
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

  ; FIXME: Do we want to push this predicate on the stack while we try to
  ; prove the `Goals_eq`? Does it ever even matter, given the sorts of predicates we generate?
  [(where #t (is-predicate? Env Predicate_1))
   (where (Env_eq Goals_eq) (equate-predicates Env Predicate_1 Predicate_2))
   (where/error Prove/Stacks_eq (apply-substitution-from-env Env_eq Prove/Stacks))
   (prove-all Env_eq Prove/Stacks_eq Goals_eq Env_out)
   --------------- "clause-fact"
   (clause-proves Env Prove/Stacks Prove/Coinductive Predicate_1 Predicate_2 Env_out)
   ]

  ; FIXME: We are inconsistent with the previous rule about whether to push `Predicate` on the
  ; stack while proving the goals that result from equating it. Seems bad.
  [(where (Env_eq (Goal_eq ...)) (equate-predicates Env Predicate_1 Predicate_2))
   (where/error Prove/Stacks_pushed (push-on-stack Prove/Stacks Prove/Coinductive Predicate_2))
   (where/error ((Goal_subst ...) Prove/Stacks_subst) (apply-substitution-from-env Env_eq (Goals Prove/Stacks_pushed)))
   (prove-all Env_eq Prove/Stacks_subst (Goal_eq ... Goal_subst ...) Env_out)
   --------------- "clause-backchain"
   (clause-proves Env Prove/Stacks Prove/Coinductive (implies Goals Predicate_1) Predicate_2 Env_out)
   ]

  [(where/error (Env_i Clause_i VarIds_i) (instantiate-quantified Env (∃ KindedVarIds Clause)))
   (clause-proves Env_i Prove/Stacks Prove/Coinductive Clause_i Predicate Env_out)
   --------------- "clause-forall"
   (clause-proves Env Prove/Stacks Prove/Coinductive (∀ KindedVarIds Clause) Predicate (reset Env VarIds_i Env_out))
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

(define-metafunction formality-logic
  ;; Returns a version of `Env_new` that has the universe and hypotheses of
  ;; `Env_old`.
  ;;
  ;; Note that the result may still contain variables declared in the old universes.
  reset : Env_old VarIds_new Env_new -> Env

  [(reset
    (Hook Universe_old _ _ _ Hypotheses_old) ; Env_old
    (VarId_new ...) ; VarIds_new
    (Hook _ VarBinders_new Substitution_new VarInequalities_new _) ; Env_new
    )
   (Hook Universe_old VarBinders_new Substitution_new VarInequalities_new Hypotheses_old)
   ]
  )

(module+ test
  (require "../test/hook.rkt")

  (define-syntax-rule (test-can-prove env goal)
    (test-equal
     (judgment-holds (prove-top-level-goal/cosld env goal _))
     #t)
    )

  (define-syntax-rule (test-cannot-prove env goal)
    (test-equal
     (judgment-holds (prove-top-level-goal/cosld env goal _))
     #f)
    )

  (redex-let*
   formality-logic
   ((; A is in U0
     (Env_0 Term_A (VarId_0)) (term (instantiate-quantified EmptyEnv (∃ ((type A)) A))))
    (; V is a placeholder in U1
     (Env_1 Term_T (VarId_1)) (term (instantiate-quantified Env_0 (∀ ((type T)) T))))
    (; X is in U1
     (Env_2 Term_X (VarId_2)) (term (instantiate-quantified Env_1 (∃ ((type X)) X))))
    (; Y, Z are in U1
     (Env_3 (Term_Y Term_Z) VarIds_3) (term (instantiate-quantified Env_2 (∃ ((type Y) (type Z)) (Y Z)))))
    ((Env_4 ()) (term (relate-parameters Env_3 ((rigid-ty Vec (Term_A)) == (rigid-ty Vec (Term_X))))))
    (Env_5 (term (reset Env_2 VarIds_3 Env_4)))
    )

   ;; check that X was originally in U1 but was reduced to U0,
   ;; and that reduction survived the "reset-env" call
   (test-equal (term (universe-of-var-in-env Env_2 Term_X)) (term (next-universe RootUniverse)))
   (test-equal (term (universe-of-var-in-env Env_4 Term_X)) (term RootUniverse))
   (test-equal (term (universe-of-var-in-env Env_5 Term_X)) (term RootUniverse))

   (traced '()
           (test-can-prove EmptyEnv (&& ())))

   (redex-let*
    formality-logic
    ((Env (term (env-with-vars-in-current-universe EmptyEnv ∃ ((type T) (type U) (type V))))))
    (traced '()
            (test-equal
             (judgment-holds (prove-top-level-goal-substitution
                              Env
                              (&& ((T == (rigid-ty Vec (U)))
                                   (U == (rigid-ty Vec (V)))
                                   (V == (rigid-ty i32 ()))))
                              Substitution_out)
                             Substitution_out)
             (term (((V (rigid-ty i32 ()))
                     (U (rigid-ty Vec ((rigid-ty i32 ()))))
                     (T (rigid-ty Vec ((rigid-ty Vec ((rigid-ty i32 ()))))))
                     )))))
    )

   (test-can-prove
    EmptyEnv
    (∀ ((type T))
       (implies ((is-implemented (Debug (T))))
                (is-implemented (Debug (T))))))

   (test-cannot-prove
    (env-with-vars-in-current-universe EmptyEnv ∃ ((type X)))
    (∃ ((type X))
       (∀ ((type T))
          (T == X))))

   (test-cannot-prove
    (env-with-vars-in-current-universe EmptyEnv ∃ ((type X)))
    (∀ ((type T))
       (T == X)))

   (test-can-prove
    EmptyEnv
    (∀ ((type T))
       (∃ ((type X))
          (T == X))))

   (redex-let*
    formality-logic
    ((Invariant_PartialEq-if-Eq (term (∀ ((type T)) (implies ((is-implemented (Eq (T))))
                                                             (is-implemented (PartialEq (T)))))))
     (Env (term (env-with-clauses-and-invariants ()
                                                 (Invariant_PartialEq-if-Eq)
                                                 ))))

    (test-cannot-prove Env (is-implemented (PartialEq ((rigid-ty u32 ())))))

    (test-can-prove Env
                    (∀ ((type T)) (implies ((is-implemented (Eq (T))))
                                           (is-implemented (PartialEq (T))))))
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
  ; * Then we can try to prove `P`, based on the hypothesis `P => Q`...
  ; * Which requires proving `P`, based on the hypothesis `P => P`...
  ; * ...which succeeds, because we have a cycle! Uh oh.
  ;
  ; Except it doesn't, because cycles that involve hypotheses are inductive
  ; and thus rejected.
  (redex-let*
   formality-logic
   ((Clause (term (implies (q) p)))
    (Invariant (term (∀ () (implies (p) q))))
    (Hypothesis (term (implies (p) p)))
    (Env (term (env-with-clauses-and-invariants (Clause)
                                                (Invariant)
                                                )))
    )

   (traced '()
           (test-cannot-prove Env (implies (Hypothesis) q)))
   )

  ; Version of the above where our hypothesis is just `p`; this should be provable.
  (redex-let*
   formality-logic
   ((Clause (term (implies (q) p)))
    (Invariant (term (∀ () (implies (p) q))))
    (Env (term (env-with-clauses-and-invariants (Clause)
                                                (Invariant)
                                                )))
    )

   (traced '()
           (test-can-prove Env (implies (p) q)))
   )

  )
