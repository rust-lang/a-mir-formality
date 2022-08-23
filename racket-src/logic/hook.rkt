#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt")
(provide formality-logic-hook
         env-clauses-for-predicate
         env-invariants
         elaborate-relation
         relate-parameters
         solve-builtin-predicate
         debone-predicate
         categorize-goal
         is-predicate?
         is-relation?
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
;; * `categorize-goal`: given a `Goal` returns a `Goal/Categorization` based on the refined grammar
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
      (categorize-goal-fn (term Goal)))
   (where/error (Hook: Term_hook) (env-hook Env))
   ]
  )


(define-metafunction formality-logic
  ;; True if this `Goal` is a user-predicate or builtin-predicate
  is-predicate? : Env Goal -> boolean

  [(is-predicate? Env Goal)
   #t
   (where (user-predicate _) (categorize-goal Env Goal))
   ]

  [(is-predicate? Env Goal)
   #t
   (where builtin-predicate (categorize-goal Env Goal))
   ]

  [(is-predicate? Env Goal)
   #f
   ]
  )

(define-metafunction formality-logic
  ;; True if this `Goal` is a builtin relation
  is-relation? : Env Goal -> boolean

  [(is-relation? Env Goal)
   #t
   (where builtin-relation (categorize-goal Env Goal))
   ]

  [(is-relation? Env Goal)
   #f
   ]
  )


(define-metafunction formality-logic
  ;; True if this `Goal` is a predicate or relation
  is-atomic-goal? : Env Goal -> boolean

  [(is-atomic-goal? Env Goal)
   (any? (is-relation? Env Goal)
         (is-predicate? Env Goal))
   ]

  )