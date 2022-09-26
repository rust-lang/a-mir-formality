#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "env.rkt"
         "substitution.rkt"
         )
(provide formality-logic-hook
         env-clauses-for-predicate
         env-invariants
         elaborate-relation
         relate-parameters
         solve-builtin-predicate
         debone-predicate
         categorize-goal
         is-predicate-goal?
         is-predicate-hypothesis?
         is-relation-goal?
         is-relation-hypothesis?
         is-atomic-goal?
         )

;; Creates a "hook" value:
;;
;; * `clauses`: a `Env Predicate -> Clauses` lambda that returns all clauses that could possibly
;;   prove `Predicate`.
;; * `invariants`: a `-> Invariants` lambda that returns all the invariants in the program
;; * `elaborate-relations`: a `Env Relation -> AtomicGoals` lambda that takes a relation and finds the things that are implied by it
;; * `relate-parameters`: a `Env Relation -> (Env Goals) or Error` lambda that relates two parameters
;; * `solve-builtin-predicate`: a `Env Predicate -> (Env Goals) or Error` lambda that solves a builtin predicate
;; * `debone-predicate`: a `Predicate -> Predicate/Deboned` function that separates into skeleton, parameters
;; * `categorize-goal`: given `Env Goal` returns a `Goal/Categorization` based on the refined grammar
;;                      (the goal will always be fully updated w/r/t inference variables)
(struct formality-logic-hook (clauses
                              invariants
                              elaborate-relation
                              relate-parameters
                              solve-builtin-predicate
                              debone-predicate
                              categorize-goal
                              ))

(define-metafunction formality-logic
  ;; Returns all program clauses in the environment that are
  ;; potentially relevant to solving `Predicate`
  ;;
  ;; Defined by the decl layer.
  env-clauses-for-predicate : Env Predicate -> Clauses

  [(env-clauses-for-predicate Env Predicate)
   ,(let ((clauses-fn (formality-logic-hook-clauses (term any))))
      (clauses-fn (term Env) (term Predicate)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Returns the invariants in the environment
  ;;
  ;; Defined by the decl layer.
  env-invariants : Env -> Invariants

  [(env-invariants Env)
   ,(let ((invariants-fn (formality-logic-hook-invariants (term any))))
      (invariants-fn))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Given a relation that is in the environment, elaborates to new
  ;; hypotheses that follow from that relation. (e.g., `&'a T: 'b`
  ;; implies `'a: 'b` and `T: 'b`).
  ;;
  ;; Defined by the ty layer.
  elaborate-relation : Env Relation -> AtomicGoals

  [(elaborate-relation Env Relation)
   ,(let ((fn (formality-logic-hook-elaborate-relation (term any))))
      (fn (term Env) (term Relation)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Apply the relation `Relation`, potentially binding existential variables in `Env`
  ;;
  ;; Defined by the ty layer.
  relate-parameters : Env Relation -> (Env Goals) or Error

  [(relate-parameters Env Relation)
   ,(let ((relate-fn (formality-logic-hook-relate-parameters (term any))))
      (relate-fn (term Env) (term Relation)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Apply the relation `Relation`, potentially binding existential variables in `Env`
  ;;
  ;; Defined by the decl layer.
  solve-builtin-predicate : Env Predicate -> (Env Goals) or Error

  [(solve-builtin-predicate Env Predicate)
   ,(let ((solve-builtin-predicate-fn (formality-logic-hook-solve-builtin-predicate (term any))))
      (solve-builtin-predicate-fn (term Env) (term Predicate)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Separate a predicate into its skeleton and a list of parameters.
  ;;
  ;; Defined by the ty layer.
  debone-predicate : Env Predicate -> Predicate/Deboned

  [(debone-predicate Env Predicate)
   ,(let ((debone-predicate-fn (formality-logic-hook-debone-predicate (term any))))
      (debone-predicate-fn (term Predicate)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Categories the `Goal` according to the grammar defined by the higher-level layers
  ;; (e.g., ty, decl)
  ;;
  ;; Defined by the decl layer.
  categorize-goal : Env Goal -> Goal/Categorization

  [(categorize-goal Env Goal)
   ,(let ((categorize-goal-fn (formality-logic-hook-categorize-goal (term Term_hook))))
      (categorize-goal-fn (term Env) (term Goal_1)))
   (where/error (Hook: Term_hook) (env-hook Env))
   (where/error Goal_1 (apply-substitution-from-env Env Goal))
   ]
  )

(define-metafunction formality-logic
  ;; True if this `Hypothesis` is a user-predicate or builtin-predicate
  is-predicate-hypothesis? : Env Hypothesis -> boolean

  [(is-predicate-hypothesis? Env BuiltinHypothesis)
   #f
   ]

  ; SUBTLE ORDERING: `Predicate` is defined in the logic grammar as `Term`,
  ; so this arm *always* matches (as does the next one).
  ; This is why we match `BuiltinHypothesis` first, to screen out `Hypothesis`
  ; that would not *also* match as goals. Then we invoke the `categorize-goal` hook to
  ; distinguish between predicates and relations.

  [(is-predicate-hypothesis? Env Predicate)
   #t
   (where (user-predicate _) (categorize-goal Env Predicate))
   ]

  [(is-predicate-hypothesis? Env Predicate)
   #t
   (where builtin-predicate (categorize-goal Env Predicate))
   ]

  [(is-predicate-hypothesis? Env AtomicGoal)
   #f
   ]
  )

(define-metafunction formality-logic
  ;; True if this `Goal` is a user-predicate or builtin-predicate
  is-predicate-goal? : Env Goal -> boolean

  [(is-predicate-goal? Env BuiltinGoal)
   #f
   ]

  ; SUBTLE: `Predicate` is defined in the logic grammar as `Term`,
  ; so this arm *always* matches (as does the next one).
  ; We invoke the `categorize-goal` hook to
  ; distinguish between predicates and relations.

  [(is-predicate-goal? Env AtomicGoal)
   #t
   (where (user-predicate _) (categorize-goal Env AtomicGoal))
   ]

  [(is-predicate-goal? Env AtomicGoal)
   #t
   (where builtin-predicate (categorize-goal Env AtomicGoal))
   ]

  [(is-predicate-goal? Env Goal)
   #f
   ]
  )

(define-metafunction formality-logic
  ;; True if this `Hypothesis` is a user-predicate or builtin-predicate
  is-relation-hypothesis? : Env Hypothesis -> boolean

  [(is-relation-hypothesis? Env BuiltinHypothesis)
   #f
   ]

  ; SUBTLE ORDERING: `Predicate` is defined in the logic grammar as `Term`,
  ; so this arm *always* matches (as does the next one).
  ; This is why we match `BuiltinHypothesis` first, to screen out `Hypothesis`
  ; that would not *also* match as goals. Then we invoke the `categorize-goal` hook to
  ; distinguish between predicates and relations.

  [(is-relation-hypothesis? Env AtomicGoal)
   #t
   (where builtin-relation (categorize-goal Env AtomicGoal))
   ]

  ; Relation is also very broadly defined.
  ; It doesn't *always* match, but it matches any 3-tuple.
  [(is-relation-hypothesis? Env _)
   #f
   ]
  )

(define-metafunction formality-logic
  ;; True if this `Goal` is a builtin relation
  is-relation-goal? : Env Goal -> boolean

  [(is-relation-goal? Env BuiltinGoal)
   #f
   ]

  ; SUBTLE: `Relation` is defined in the logic grammar as,
  ; effectively, `(Term Term Term)`,
  ; so must invoke the `categorize-goal` hook to
  ; distinguish relations.

  [(is-relation-goal? Env Goal)
   #t
   (where builtin-relation (categorize-goal Env Goal))
   ]

  [(is-relation-goal? Env Goal)
   #f
   ]
  )

(define-metafunction formality-logic
  ;; True if this `Goal` is a predicate or relation
  is-atomic-goal? : Env Goal -> boolean

  [(is-atomic-goal? Env Goal)
   (any? (is-relation-goal? Env Goal)
         (is-predicate-goal? Env Goal))
   ]

  )