#lang racket
(require redex "grammar.rkt" "substitution.rkt")
(provide prove)

(define-judgment-form patina-ty
  #:mode (prove I I)
  #:contract (prove env goal)

  [(where ((_ ... program-clause _ ... ) _) env)
   (program-clause-proves env program-clause predicate)
   --------------- "prove-program-clause"
   (prove env predicate)
   ]

  [(where (_ (_ ... hypothesis _ ... )) env)
   (hypothesis-implies env hypothesis predicate)
   --------------- "prove-hypothesis"
   (prove env predicate)
   ]

  [(prove env goal) ...
   --------------- "prove-all"
   (prove env (all (goal ...)))
   ]

  [(prove env goal_1)
   --------------- "prove-any"
   (prove env (any (goal_0 ... goal_1 goal_2 ...)))
   ]

  [(prove (program-clauses (hypothesis_0 ... hypothesis_1 ...)) goal)
   --------------- "prove-implies"
   (prove (program-clauses (hypothesis_0 ...)) (implies (hypothesis_1 ...) goal))
   ]

  [; FIXME: universes, etc
   (where/error substitution-map (substitution-to-fresh-vars env goal kinded-var-ids))
   (where/error goal_1 (apply-substitution substitution-map goal))
   (prove env goal_1)
   --------------- "prove-forall"
   (prove env (forall kinded-var-ids goal))
   ]

  [; FIXME: unifiers
   (prove env (all kinded-var-ids goal))
   --------------- "prove-exists"
   (prove env (exists kinded-var-ids goal))
   ]

  )

(define-judgment-form patina-ty
  #:mode (program-clause-proves I I I)
  #:contract (program-clause-proves env program-clause predicate)

  [--------------- "pc-fact"
   (program-clause-proves env predicate predicate)
   ]

  [(prove env (all goals))
   --------------- "pc-backchain"
   (program-clause-proves env (implies goals predicate) predicate)
   ]

  [; FIXME: unifiers
   (where/error substitution-map (substitution-to-fresh-vars (env program-clause predicate) kinded-var-ids))
   (where/error program-clause_1 (apply-substitution substitution-map program-clause))
   (program-clause-proves env program-clause_1 predicate)
   --------------- "pc-forall"
   (program-clause-proves env (forall kinded-var-ids program-clause) predicate)
   ]

  )

(define-judgment-form patina-ty
  #:mode (hypothesis-implies I I I)
  #:contract (hypothesis-implies env hypothesis goal)

  [--------------- "hypothesized"
   (hypothesis-implies env predicate predicate)
   ]

  [(where (_ (_ ... hypothesis _ ...)) env)
   (hypothesis-implies env hypothesis predicate_0)
   --------------- "hypothesized-backchain"
   (hypothesis-implies env (implies predicate_0 predicate_1) predicate_1)
   ]

  [; FIXME: unifiers
   (where/error substitution-map (substitution-to-fresh-vars (env hypothesis predicate) kinded-var-ids))
   (where/error hypothesis_1 (apply-substitution substitution-map hypothesis))
   (hypothesis-implies env hypothesis_1 predicate)
   --------------- "hypothesized-forall"
   (hypothesis-implies env (forall kinded-var-ids hypothesis) predicate)
   ]
  )

(module+ test
  (test-equal (judgment-holds (prove (() ()) (all ())))
              #t
              )
  )