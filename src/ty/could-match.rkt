#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide ty:predicates-could-match
         )

(define-metafunction formality-ty
  ty:predicates-could-match : Predicate_1 Predicate_2 -> boolean

  [(ty:predicates-could-match (Implemented (TraitId_!_0 _)) (Implemented (TraitId_!_0 _)))
   #f]

  [(ty:predicates-could-match (Implemented _) (HasImpl _))
   #f
   ]

  [(ty:predicates-could-match (Implemented _) (WellFormed _))
   #f
   ]

  [(ty:predicates-could-match (HasImpl (TraitId_!_0 _)) (HasImpl (TraitId_!_0 _)))
   #f
   ]

  [(ty:predicates-could-match (HasImpl _) (Implemented _))
   #f
   ]

  [(ty:predicates-could-match (HasImpl _) (WellFormed _))
   #f
   ]

  [(ty:predicates-could-match (WellFormed (ParameterKind_!_0 _)) (WellFormed (ParameterKind_!_0 _)))
   #f
   ]

  [(ty:predicates-could-match (WellFormed _) (Implemented _))
   #f
   ]

  [(ty:predicates-could-match (WellFormed _) (HasImpl _))
   #f
   ]

  [(ty:predicates-could-match Predicate _)
   #t
   ]

  )

(module+ test
  (test-equal (term (ty:predicates-could-match
                     (Implemented (Debug (T)))
                     (Implemented (Debug ((scalar-ty i32))))))
              (term #t))

  (test-equal (term (ty:predicates-could-match
                     (Implemented (Debug (T)))
                     (Implemented (WithDebug ((scalar-ty i32))))))
              (term #f))

  (test-equal (term (ty:predicates-could-match
                     (Implemented (Debug (T)))
                     (HasImpl (WithDebug ((scalar-ty i32))))))
              (term #f))
  )

