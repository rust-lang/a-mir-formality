#lang racket
(require redex/reduction-semantics "grammar.rkt" "env.rkt")
(provide formality-logic-hook
         env-clauses-for-predicate
         env-invariants
         relate-parameters
         debone-predicate
         is-predicate?
         is-relation?
         is-atomic-goal?
         )

;; Creates a "hook" value:
;;
;; * `clauses`: a `Predicate -> Clauses` lambda that returns all clauses that could possibly
;;   prove `Predicate`.
;; * `invariants`: a `-> Invariants` lambda that returns all the invariants in the program
;; * `relate-parameters`: a `Env Relation -> Env or Error` lambda that relates two parameters
;; * `is-predicate`: true if a term matches `Predicate`, needed because logic/grammar has `Predicate ::= Term`
;; * `debone-predicate`: a `Predicate -> Predicate/Deboned` function that separates into skeleton, parameters
;; * `is-relation`: true if a term matches `Relation`, needed because logic/grammar has `Relation ::= Term`
(struct formality-logic-hook (clauses
                              invariants
                              relate-parameters
                              is-predicate
                              debone-predicate
                              is-relation
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
   ,(let ((invariants-fn (formality-logic-hook-invariants (term any))))
      (invariants-fn))
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
  ;; Separate a predicate into its skeleton and a list of parameters.
  debone-predicate : Env Predicate -> Predicate/Deboned

  [(debone-predicate Env Predicate)
   ,(let ((debone-predicate-fn (formality-logic-hook-debone-predicate (term any))))
      (debone-predicate-fn (term Predicate)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; The "grammar" for predicates is just *any term* -- that's not very
  ;; useful, and extension languages refine it. When matching against predicates,
  ;; then, we can use this function to avoid matching on other kinds of goals.
  is-predicate? : Env Goal -> boolean

  [(is-predicate? Env Goal)
   ,(let ((is-predicate-fn (formality-logic-hook-is-predicate (term any))))
      (is-predicate-fn (term Goal)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; The "grammar" for predicates is just *any term* -- that's not very
  ;; useful, and extension languages refine it. When matching against predicates,
  ;; then, we can use this function to avoid matching on other kinds of goals.
  is-relation? : Env Goal -> boolean

  [(is-relation? Env Goal)
   ,(let ((is-relation-fn (formality-logic-hook-is-relation (term any))))
      (is-relation-fn (term Goal)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-logic
  ;; The "grammar" for predicates is just *any term* -- that's not very
  ;; useful, and extension languages refine it. When matching against predicates,
  ;; then, we can use this function to avoid matching on other kinds of goals.
  is-atomic-goal? : Env Goal -> boolean

  [(is-atomic-goal? Env Goal)
   ,(or (term (is-predicate? Env Goal)) (term (is-relation? Env Goal)))
   ]
  )