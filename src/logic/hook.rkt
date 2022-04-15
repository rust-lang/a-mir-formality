#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt")
(provide formality-logic-hook
         env-clauses-for-predicate
         env-invariants
         equate-predicates
         equate-predicates/vars
         relate-parameters
         predicates-could-match
         is-predicate-goal?
         )

;; Creates a "hook" value:
;;
;; * `clauses`: a `Predicate -> Clauses` lambda that returns all clauses that could possibly
;;   prove `Predicate`.
;; * `invariants`: a `(Invariant ...)` term with all the invariants in the program
;; * `equate-predicates/vars`: a `Env VarIds Predicate Predicate -> Env or Error` lambda that unifies two predicates
;; * `relate-parameters`: a `Env Relation -> Env or Error` lambda that relates two parameters
;; * `predicate-could-match`: a `Predicate Predicate -> bool` that filters down predicates
;; * `is-predicate`: true if a term matches `Predicate`
(struct formality-logic-hook (clauses
                              invariants
                              equate-predicates/vars
                              relate-parameters
                              predicates-could-match
                              is-predicate
                              ))

(define-metafunction formality-logic
  ;; Returns all program clauses in the environment that are
  ;; potentially relevant to solving `Predicate`
  env-clauses-for-predicate : Env Predicate -> Clauses

  [(env-clauses-for-predicate Env Predicate)
   ,(let ((clauses-fn (formality-logic-hook-clauses (term any))))
      (clauses-fn (term Predicate)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Returns the invariants in the environment
  env-invariants : Env -> Invariants

  [(env-invariants Env)
   ,(formality-logic-hook-invariants (term any))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Equate `Predicate_1` and `Predicate_2` by binding existential variables in `Env`
  equate-predicates : Env Predicate Predicate -> (Env Goals) or Error

  [(equate-predicates Env Predicate_1 Predicate_2)
   (equate-predicates/vars Env VarIds Predicate_1 Predicate_2)
   (where/error VarIds  (existential-vars-in-env Env))]

  )

(define-metafunction formality-logic
  ;; Equate `Predicate_1` and `Predicate_2` by binding `VarIds` in `Env`
  equate-predicates/vars : Env VarIds Predicate_1 Predicate_2 -> (Env Goals) or Error

  [(equate-predicates/vars Env VarIds Predicate_1 Predicate_2)
   ,(let ((equate-fn (formality-logic-hook-equate-predicates/vars (term any))))
      (equate-fn (term Env) (term VarIds) (term Predicate_1) (term Predicate_2)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; Apply the relation `Relation`, potentially binding existential variables in `Env`
  relate-parameters : Env Relation -> (Env Goals) or Error

  [(relate-parameters Env Relation)
   ,(let ((relate-fn (formality-logic-hook-relate-parameters (term any))))
      (relate-fn (term Env) (term Relation)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  predicates-could-match : Env Predicate_1 Predicate_2 -> boolean

  [(predicates-could-match Env Predicate_1 Predicate_2)
   ,(let ((predicates-could-match-fn (formality-logic-hook-predicates-could-match (term any))))
      (predicates-could-match-fn (term Predicate_1) (term Predicate_2)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; The "grammar" for predicates is just *any term* -- that's not very
  ;; useful, and extension languages refine it. When matching against predicates,
  ;; then, we can use this function to avoid matching on other kinds of goals.
  is-predicate-goal? : Env Goal -> boolean

  [(is-predicate-goal? Env Goal)
   ,(let ((is-predicate-fn (formality-logic-hook-is-predicate (term any))))
      (is-predicate-fn (term Goal)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )