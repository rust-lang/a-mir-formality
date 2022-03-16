#lang racket
(require redex
         "../grammar.rkt")

(provide filter-clauses)

(define-metafunction formality-ty
  ;; Filters a list of clauses down to clauses that could *plausibly* match
  ;; Predicate. This is an optimization and honestly mostly to help when
  ;; reading debugging traces.
  filter-clauses : Clauses Predicate -> Clauses

  [(filter-clauses () Predicate)
   ()
   ]

  [(filter-clauses (Clause_next Clause_rest ...) Predicate)
   (Clause_next Clause_rest_filtered ...)
   (where #t (clause-could-match-predicate Clause_next Predicate))
   (where/error (Clause_rest_filtered ...) (filter-clauses (Clause_rest ...) Predicate))
   ]

  [(filter-clauses (Clause_next Clause_rest ...) Predicate)
   (filter-clauses (Clause_rest ...) Predicate)
   (where #f (clause-could-match-predicate Clause_next Predicate))
   ]
  )

(define-metafunction formality-ty
  clause-could-match-predicate : Clause Predicate -> boolean

  ;; Rule out various things that are obviously not going to unify.
  ;;
  ;; We can add more cases here. The most obvious improvement would be
  ;; ruling out `Foo<T>: Debug` vs `Bar<T>: Debug` etc.
  ;;
  ;; I've deliberately structured this to default to NOT filtering so
  ;; that it is less likely to cause really confusing bugs by filtering out
  ;; something we need. The only harm to not filtering is harder-to-read debug
  ;; output.

  [(clause-could-match-predicate (Implemented (TraitId_!_0 _)) (Implemented (TraitId_!_0 _)))
   #f
   ]

  [(clause-could-match-predicate (Implemented _) (HasImpl _))
   #f
   ]

  [(clause-could-match-predicate (Implemented _) (WellFormed _))
   #f
   ]

  [(clause-could-match-predicate (HasImpl (TraitId_!_0 _)) (HasImpl (TraitId_!_0 _)))
   #f
   ]

  [(clause-could-match-predicate (HasImpl _) (Implemented _))
   #f
   ]

  [(clause-could-match-predicate (HasImpl _) (WellFormed _))
   #f
   ]

  [(clause-could-match-predicate (WellFormed (ParameterKind_!_0 _)) (WellFormed (ParameterKind_!_0 _)))
   #f
   ]

  [(clause-could-match-predicate (WellFormed _) (Implemented _))
   #f
   ]

  [(clause-could-match-predicate (WellFormed _) (HasImpl _))
   #f
   ]

  [(clause-could-match-predicate Predicate _)
   #t
   ]

  [(clause-could-match-predicate (Implies Goals Predicate) Predicate_1)
   (clause-could-match-predicate Predicate Predicate_1)
   ]

  [(clause-could-match-predicate (ForAll KindedVarIds Clause) Predicate_1)
   (clause-could-match-predicate Clause Predicate_1)
   ]

  )

(module+ test

  (redex-let*
   formality-ty

   ((Clauses_test (term ((ForAll ((TyKind T)) (Implemented (Debug (T))))))))

   (test-equal (term (filter-clauses
                      Clauses_test
                      (Implemented (Debug ((scalar-ty i32))))))
               (term Clauses_test))

   (test-equal (term (filter-clauses
                      Clauses_test
                      (Implemented (WithDebug ((scalar-ty i32))))))
               (term ()))

   (test-equal (term (filter-clauses
                      Clauses_test
                      (HasImpl (Debug ((scalar-ty i32))))))
               (term ()))
   )
  )