#lang racket
(require redex/reduction-semantics "../grammar.rkt" "../env.rkt" "../hook.rkt" "unify.rkt")
(provide env-with-clauses-and-invariants
         EmptyEnv
         )

(define-metafunction formality-logic
  ;; Creates a suitable env for testing the formality-logic layer
  ;; that has the given clauses + invariants as the program.
  env-with-clauses-and-invariants : Clauses Invariants -> Env

  [(env-with-clauses-and-invariants Clauses Invariants)
   (env-with-hook (Hook: ,(begin
                            (formality-logic-hook (lambda (predicate) (term Clauses))
                                                  (lambda () (term Invariants))
                                                  (lambda (env predicate1 predicate2)
                                                    (term (test-equate-predicates ,env ,predicate1 ,predicate2)))
                                                  (lambda (env relation)
                                                    (term (test-relate-parameters ,env ,relation)))
                                                  (lambda (predicate1 predicate2) #t)
                                                  (lambda (goal)
                                                    (term (logic:is-predicate? ,goal))
                                                    )
                                                  (lambda (goal)
                                                    (term (logic:is-relation-goal? ,goal))
                                                    )
                                                  ))))
   ]
  )

;; A suitable env for testing the formality-logic layer in isolation
;; with no clauses nor invariants.
(define-term EmptyEnv (env-with-clauses-and-invariants () ()))

(define-metafunction formality-logic
  ;; The "grammar" for predicates is just *any term* -- that's not very
  ;; useful, and extension languages refine it. When matching against predicates,
  ;; then, we can use this function to avoid matching on other kinds of goals.
  logic:is-predicate? : Goal -> boolean

  [(logic:is-predicate? BuiltinGoal) #f]
  [(logic:is-predicate? (Parameter_1 == Parameter_2)) #f]
  [(logic:is-predicate? _) #t]
  )

(define-metafunction formality-logic
  ;; At this layer, only `==` is a recognized relation
  logic:is-relation-goal? : Goal -> boolean

  [(logic:is-relation-goal? (Parameter_1 == Parameter_2)) #t]
  [(logic:is-relation-goal? _) #f]
  )