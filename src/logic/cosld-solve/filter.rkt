#lang racket
(require redex
         "../grammar.rkt"
         "../hook.rkt")

(provide filter-clauses)

(define-metafunction formality-logic
  ;; Filters a list of clauses down to clauses that could *plausibly* match
  ;; Predicate. This is an optimization and honestly mostly to help when
  ;; reading debugging traces.
  filter-clauses : Env Clauses Predicate -> Clauses

  [(filter-clauses Env () Predicate)
   ()
   ]

  [(filter-clauses Env (Clause_next Clause_rest ...) Predicate)
   (Clause_next Clause_rest_filtered ...)
   (where #t (clause-could-match-predicate Env Clause_next Predicate))
   (where/error (Clause_rest_filtered ...) (filter-clauses Env (Clause_rest ...) Predicate))
   ]

  [(filter-clauses Env (Clause_next Clause_rest ...) Predicate)
   (filter-clauses Env (Clause_rest ...) Predicate)
   (where #f (clause-could-match-predicate Env Clause_next Predicate))
   ]
  )

(define-metafunction formality-logic
  clause-could-match-predicate : Env Clause Predicate -> boolean

  ;; Rule out various things that are obviously not going to unify.
  ;;
  ;; We can add more cases here. The most obvious improvement would be
  ;; ruling out `Foo<T>: Debug` vs `Bar<T>: Debug` etc.
  ;;
  ;; I've deliberately structured this to default to NOT filtering so
  ;; that it is less likely to cause really confusing bugs by filtering out
  ;; something we need. The only harm to not filtering is harder-to-read debug
  ;; output.

  [(clause-could-match-predicate Env (implies Goals Predicate) Predicate_1)
   (clause-could-match-predicate Env Predicate Predicate_1)
   ]

  [(clause-could-match-predicate Env (∀ KindedVarIds Clause) Predicate_1)
   (clause-could-match-predicate Env Clause Predicate_1)
   ]

  [(clause-could-match-predicate Env Predicate_1 Predicate_2)
   (predicates-could-match Env Predicate_1 Predicate_1)
   ]

  )

(module+ test
  (require "../test/hook.rkt")

  (redex-let*
   formality-logic

   ((Clauses_test (term ((∀ ((type T)) (Implemented (Debug (T))))))))

   (test-equal (term (filter-clauses
                      EmptyEnv
                      Clauses_test
                      (Implemented (Debug ((scalar-ty i32))))))
               (term Clauses_test))

   ; The "filtering" function we use at the logic level is ... not very precise.
   ; (It always returns #t)

   (test-equal (term (filter-clauses
                      EmptyEnv
                      Clauses_test
                      (Implemented (WithDebug ((scalar-ty i32))))))
               (term Clauses_test))

   (test-equal (term (filter-clauses
                      EmptyEnv
                      Clauses_test
                      (HasImpl (Debug ((scalar-ty i32))))))
               (term Clauses_test))
   )
  )