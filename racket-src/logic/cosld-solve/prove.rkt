#lang racket
(require redex/reduction-semantics
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
  #:contract (prove-top-level-goal/cosld Env Goal EnvOutput)

  [(prove Env (() ()) Goal EnvOutput)
   ---------------
   (prove-top-level-goal/cosld Env Goal EnvOutput)
   ]
  )

(define-judgment-form formality-logic
  #:mode (prove I I I O)
  #:contract (prove Env Prove/Stacks Goal EnvOutput)

  [(where (user-predicate Prove/Coinductive) (categorize-goal Env Predicate))
   (not-in-stacks Env Predicate Prove/Stacks)
   (where (_ ... Clause _ ... ) (filter-clauses Env (env-clauses-for-predicate Env Predicate) Predicate))
   (clause-proves Env Prove/Stacks Prove/Coinductive Clause Predicate EnvOutput)
   --------------- "prove-clause"
   (prove Env Prove/Stacks Predicate EnvOutput)
   ]

  [(where builtin-predicate (categorize-goal Env Predicate))
   (not-in-stacks Env Predicate Prove/Stacks)
   (where (Env_builtin Goals_builtin) (solve-builtin-predicate Env Predicate)) ; could also be Error
   (prove-all Env_builtin Prove/Stacks Goals_builtin EnvOutput)
   --------------- "prove-builtin-clause"
   (prove Env Prove/Stacks Predicate EnvOutput)
   ]

  [(where #t (is-predicate-goal? Env Predicate))
   (not-in-stacks Env Predicate Prove/Stacks)
   (where (_ ... Hypothesis _ ... ) (filter-clauses Env (env-hypotheses (elaborate-hypotheses Env)) Predicate))
   (clause-proves Env Prove/Stacks - Hypothesis Predicate EnvOutput)
   --------------- "prove-hypotheses-imply"
   (prove Env Prove/Stacks Predicate EnvOutput)
   ]

  [(where #t (is-predicate-goal? Env Predicate))
   (where #t (in? (apply-substitution-from-env Env Predicate)
                  (apply-substitution-from-env Env Predicates_co)))
   --------------- "prove-coinductive-cycle"
   (prove Env (_ Predicates_co) Predicate Env)
   ]

  [(where builtin-relation (categorize-goal Env Relation))
   (where (Env_eq Goals_eq) (relate-parameters Env Relation))
   (prove-all Env_eq Prove/Stacks Goals_eq EnvOutput)
   --------------- "prove-relate"
   (prove Env Prove/Stacks Relation EnvOutput)
   ]

  [(where ambiguous-goal (categorize-goal Env Predicate))
   --------------- "prove-ambiguous-goal"
   (prove Env Prove/Stacks Predicate ambiguous)
   ]

  [--------------- "prove-ambiguous"
   (prove Env Prove/Stacks ambiguous ambiguous)
   ]

  [(prove-all Env Prove/Stacks Goals EnvOutput)
   --------------- "prove-all"
   (prove Env Prove/Stacks (&& Goals) EnvOutput)
   ]

  [(prove Env Prove/Stacks Goal_1 EnvOutput)
   --------------- "prove-any"
   (prove Env Prove/Stacks (|| (Goal_0 ... Goal_1 Goal_2 ...)) EnvOutput)
   ]

  [(where Env_1 (env-with-hypotheses Env Hypotheses))
   (prove Env_1 Prove/Stacks Goal EnvOutput)
   --------------- "prove-implies"
   (prove Env Prove/Stacks (implies Hypotheses Goal) (reset Env () EnvOutput))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (∀ KindedVarIds Goal)))
   (prove Env_1 Prove/Stacks Goal_1 EnvOutput)
   --------------- "prove-forall"
   (prove Env Prove/Stacks (∀ KindedVarIds Goal) (reset Env VarIds_new EnvOutput))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (∃ KindedVarIds Goal)))
   (prove Env_1 Prove/Stacks Goal_1 EnvOutput)
   --------------- "prove-exists"
   (prove Env Prove/Stacks (∃ KindedVarIds Goal) (reset Env VarIds_new EnvOutput))
   ]

  [(where/error Env_1 (env-with-hypotheses Env [coherence-mode]))
   (prove Env_1 Prove/Stacks Goal EnvOutput)
   --------------- "prove-coherence-mode"
   (prove Env Prove/Stacks (coherence-mode Goal) EnvOutput)
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
  #:contract (prove-all Env Prove/Stacks Goals EnvOutput)

  [(prove-all-inner Env Prove/Stacks Goals no-progress [] EnvOutput)
   --------------------------------------------------------
   (prove-all Env Prove/Stacks Goals EnvOutput)]
  )

(define-judgment-form formality-logic
  ;; Helper judgment: maintains a list of deferred goals and a marker that
  ;; indicates if we made progress. When a goal yields ambiguous, it is pushed
  ;; as "deferred". Once we've processed all goals, we circle back to the deferred goals
  ;; (but only if at least *one* goal made progress, otherwise we'll just defer again).
  #:mode (prove-all-inner I I I I I O)
  #:contract (prove-all-inner Env Prove/Stacks Goals Prove/Progress Goals_deferred EnvOutput)

  [; Prove next goal (if we can)
   (where/error Goal_0s (apply-substitution-from-env Env Goal_0))
   (where/error Prove/Stacks_s (apply-substitution-from-env Env Prove/Stacks))
   (prove Env Prove/Stacks_s Goal_0s EnvOutput_0)
   (where/error (Env_1 Prove/Progress_1 Goals_defer1) (prove-all-next-state Env Goal_0 EnvOutput_0 Prove/Progress_0 Goals_defer0))
   (prove-all-inner Env_1 Prove/Stacks [Goal_1 ...] Prove/Progress_1 Goals_defer1 EnvOutput)
   --------------------------------------------------------
   (prove-all-inner Env Prove/Stacks [Goal_0 Goal_1 ...] Prove/Progress_0 Goals_defer0 EnvOutput)]

  [; If we have proven everything and have nothing deferred: done
   --------------------------------------
   (prove-all-inner Env Prove/Stacks [] Prove/Progress [] Env)]

  [; Once we've solved everything, if we made some progress, go back to the deferred goals.
   (prove-all-inner Env Prove/Stacks [Goal_defer0 Goal_defer1 ...] no-progress [] EnvOutput)
   -----------------------------------------------
   (prove-all-inner Env Prove/Stacks [] made-progress [Goal_defer0 Goal_defer1 ...] EnvOutput)]

  [; If we have proven everything but have deferred goals, and we didn't make any
   ; progress when proving things, we are stuck.
   -----------------------------------------------
   (prove-all-inner Env Prove/Stacks [] no-progress [Goal_defer0 Goal_defer1 ...] ambiguous)]

  )

(define-metafunction formality-logic
  ;; Given:
  ;; * the initial env `Env`
  ;; * the goal `Goal_0` that we tried to prove
  ;; * the output `EnvOutput` from that attempt (either a new environment or `ambiguous`)
  ;; * the previous "progress state"
  ;; * the previous list of deferred goals
  ;;
  ;; Returns:
  ;; * new progress state for proving remaining goals
  ;; * new list of deferred goals (which may now include `Goal_0`)
  ;;
  ;; Note: this metafunction exists in part as an optimization. Adding more clauses
  ;; to prove-all-inner can be dramatically slower, so this allows us to consolidate
  ;; what would (imo) be more naturally written as two clauses into one.
  prove-all-next-state : Env Goal_0 EnvOutput Prove/Progress Goals_defer -> (Env Prove/Progress Goals)

  [; if we successfully proved `Goal_0`, we don't need to defer it, and we made progress
   (prove-all-next-state _ _ Env _ Goals_defer)
   (Env made-progress Goals_defer)
   ]

  [; otherwise, we DO need to defer `Goal_0`, and we did not make any more progress than before
   (prove-all-next-state Env Goal_0 ambiguous Prove/Progress (Goal_defer ...))
   (Env Prove/Progress (Goal_defer ... Goal_0))
   ]

  )

(define-judgment-form formality-logic
  #:mode (clause-proves I I I I I O)
  #:contract (clause-proves Env Prove/Stacks Prove/Coinductive Clause Predicate EnvOutput)

  ; FIXME: Do we want to push this predicate on the stack while we try to
  ; prove the `Goals_eq`? Does it ever even matter, given the sorts of predicates we generate?
  [(where #t (is-predicate-goal? Env Predicate_1))
   (where (Env_eq Goals_eq) (equate-predicates Env Predicate_1 Predicate_2))
   (where/error Prove/Stacks_eq (apply-substitution-from-env Env_eq Prove/Stacks))
   (prove-all Env_eq Prove/Stacks_eq Goals_eq EnvOutput)
   --------------- "clause-fact"
   (clause-proves Env Prove/Stacks Prove/Coinductive Predicate_1 Predicate_2 EnvOutput)
   ]

  ; FIXME: We are inconsistent with the previous rule about whether to push `Predicate` on the
  ; stack while proving the goals that result from equating it. Seems bad.
  [(where (Env_eq [Goal_eq ...]) (equate-predicates Env Predicate_1 Predicate_2))
   (where/error Prove/Stacks_pushed (push-on-stack Prove/Stacks Prove/Coinductive Predicate_2))
   (where/error ([Goal_subst ...] Prove/Stacks_subst) (apply-substitution-from-env Env_eq (Goals Prove/Stacks_pushed)))
   (prove-all Env_eq Prove/Stacks_subst [Goal_eq ... Goal_subst ...] EnvOutput)
   --------------- "clause-backchain"
   (clause-proves Env Prove/Stacks Prove/Coinductive (implies Goals Predicate_1) Predicate_2 EnvOutput)
   ]

  [(where/error (Env_i Clause_i VarIds_i) (instantiate-quantified Env (∃ KindedVarIds Clause)))
   (clause-proves Env_i Prove/Stacks Prove/Coinductive Clause_i Predicate EnvOutput)
   --------------- "clause-forall"
   (clause-proves Env Prove/Stacks Prove/Coinductive (∀ KindedVarIds Clause) Predicate (reset Env VarIds_i EnvOutput))
   ]

  )

(define-metafunction formality-logic
  ;; Check that two predicates can be made equal (possibly by unifying
  ;; inference variables)
  equate-predicates : Env Predicate_1 Predicate_2 -> (Env Goals) or Error

  [(equate-predicates Env Predicate_1 Predicate_2)
   (equate-parameters/all Env () ((Parameter_in1 Parameter_in2) ... (Parameter_out1 Parameter_out2) ...))

   (where/error (Predicate/Skeleton Parameters_in1 -> Parameters_out1) (debone-predicate Env Predicate_1))
   (where (Predicate/Skeleton Parameters_in2 -> Parameters_out2) (debone-predicate Env Predicate_2))
   (; check predicates have same number of input/output parameters
    where ((Parameter_in1 ..._in) (Parameter_in2 ..._in) (Parameter_out1 ..._out) (Parameter_out2 ..._out))
          (Parameters_in1 Parameters_in2 Parameters_out1 Parameters_out2))
   ]

  [(equate-predicates _ _ _) Error]

  )

(define-metafunction formality-logic
  equate-parameters/all : Env Goals_accum ((Parameter_1 Parameter_2) ...) -> (Env Goals) or Error

  [(equate-parameters/all Env Goals_accum ())
   (Env Goals_accum)
   ]

  [(equate-parameters/all Env_0 (Goal_0 ...) ((Parameter_1 Parameter_2) Parameters_rest ...))
   (equate-parameters/all Env_1 (Goal_0 ... Goal_1 ...) (Parameters_rest ...))
   (where (Env_1 (Goal_1 ...)) (relate-parameters Env_0 (Parameter_1 == Parameter_2)))
   ]

  [(equate-parameters/all _ _ _) Error]

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
  reset : Env_old VarIds_new EnvOutput -> EnvOutput

  [(reset
    (Hook Universe_old _ _ _ Hypotheses_old) ; Env_old
    (VarId_new ...) ; VarIds_new
    (Hook _ VarBinders_new Substitution_new VarInequalities_new _) ; Env_new
    )
   (Hook Universe_old VarBinders_new Substitution_new VarInequalities_new Hypotheses_old)
   ]

  [(reset Env_old VarIds_new ambiguous)
   ambiguous
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

  (define-judgment-form formality-logic
    ;; Convenience judgment that extracts the Substitution from the Env for testing.
    #:mode (prove-top-level-goal-substitution I I O)
    #:contract (prove-top-level-goal-substitution Env Goal Substitution)

    [(prove Env (() ()) Goal EnvOutput)
     (where Env_out EnvOutput)
     (where/error Substitution_out (env-substitution Env_out))
     ---------------
     (prove-top-level-goal-substitution Env Goal Substitution_out)
     ]
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
   )

  (traced '()
          (test-can-prove EmptyEnv true-goal))

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