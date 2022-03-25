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
                            (formality-hook (lambda (predicate) (term Clauses))
                                            (term Invariants)
                                            (lambda (env var-ids predicate1 predicate2)
                                              (term (test-equate-predicates/vars ,env ,var-ids ,predicate1 ,predicate2)))
                                            (lambda (env relation)
                                              (term (test-relate-parameters ,env ,relation)))
                                            (lambda (predicate1 predicate2) #t)
                                            ))))
   ]
  )

;; A suitable env for testing the formality-logic layer in isolation
;; with no clauses nor invariants.
(define-term EmptyEnv (env-with-clauses-and-invariants () ()))
