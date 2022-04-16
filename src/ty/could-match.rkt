#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "predicate.rkt"
         )
(provide ty:predicates-could-match
         )

(define-metafunction formality-ty
  ty:predicates-could-match : Predicate_1 Predicate_2 -> boolean

  [(ty:predicates-could-match Predicate_1 Predicate_2)
   #t
   (where ((Predicate/Skeleton _) (Predicate/Skeleton _))
          ((debone-predicate Predicate_1) (debone-predicate Predicate_2)))
   ]

  [(ty:predicates-could-match Predicate_1 Predicate_2)
   #f
   (where ((Predicate/Skeleton_!_1 _) (Predicate/Skeleton_!_1 _))
          ((debone-predicate Predicate_1) (debone-predicate Predicate_2)))
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

