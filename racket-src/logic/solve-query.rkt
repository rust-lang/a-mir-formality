#lang racket
(require redex/reduction-semantics
         racket/set
         "grammar.rkt"
         "env.rkt"
         "solution.rkt"
         "solution-simplify.rkt"
         "solution-subsumes.rkt"
         "cosld-solve.rkt"
         )
(provide logic:solve-query)

;; Tests for this are in the rust layer.

(define-metafunction formality-logic
  ;; Compute the solutions to the given query and return them.
  logic:solve-query : Hook QueryGoal -> Solutions

  [(logic:solve-query Hook QueryGoal)
   Solutions_best

   (where/error (Env_0 Goal) (instantiate-query Hook QueryGoal))
   (where/error VarIds_query (existential-vars-in-env Env_0))
   (where/error Solutions_all ,(judgment-holds (solve-top-level-query-goal VarIds_query Env_0 Goal Solution)
                                               Solution))
   (where/error Solutions_unique (extract-unique-solutions [] Solutions_all))
   (where/error Solutions_best (remove-subsumed-solutions Hook QueryGoal Solutions_unique))
   ]

  )

(define-metafunction formality-logic
  ;; Adds `Solutions` to `Solutions_accum`, unless there is already an
  ;; alpha-equivalent solution present.
  extract-unique-solutions : Solutions_accum Solutions -> Solutions

  [(extract-unique-solutions Solutions_accum [])
   Solutions_accum
   ]

  [(extract-unique-solutions Solutions_accum [Solution_0 Solution_1 ...])
   (extract-unique-solutions Solutions_accum [Solution_1 ...])
   (where #t (in? Solution_0 Solutions_accum))
   ]

  [(extract-unique-solutions Solutions_accum [Solution_0 Solution_1 ...])
   (extract-unique-solutions [Solution_accum ... Solution_0] [Solution_1 ...])
   (where #f (in? Solution_0 Solutions_accum))
   (where/error [Solution_accum ...] Solutions_accum)
   ]
  )


(define-metafunction formality-logic
  ;; Adds `Solutions` to `Solutions_accum`, unless there is already an
  ;; alpha-equivalent solution present.
  remove-subsumed-solutions : Hook QueryGoal Solutions -> Solutions

  [(remove-subsumed-solutions Hook QueryGoal Solutions)
   (remove-subsumed-solutions Hook QueryGoal [Solution_0 ... Solution_a Solution_1 ... Solution_2 ...])
   (where [Solution_0 ... Solution_a Solution_1 ... Solution_b Solution_2 ...] Solutions)
   (where #t (solution-subsumes Hook QueryGoal Solution_a Solution_b))
   ]

  [(remove-subsumed-solutions Hook QueryGoal Solutions)
   (remove-subsumed-solutions Hook QueryGoal [Solution_0 ... Solution_1 ... Solution_b Solution_2 ...])
   (where [Solution_0 ... Solution_a Solution_1 ... Solution_b Solution_2 ...] Solutions)
   (where #t (solution-subsumes Hook QueryGoal Solution_b Solution_a))
   ]

  [(remove-subsumed-solutions Hook QueryGoal Solutions)
   Solutions
   ]
  )