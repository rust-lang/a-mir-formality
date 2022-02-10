#lang racket
(require redex "grammar.rkt" "substitution.rkt")
(provide prove)

(define-judgment-form patina-ty
  #:mode (prove I I)
  #:contract (prove Env Goal)

  [(where (_ ... Clause _ ... ) (env-clauses Env))
   (Clause-proves Env Clause Predicate)
   --------------- "prove-Clause"
   (prove Env Predicate)
   ]

  [(where (_ ... Hypothesis _ ... ) (env-hypotheses Env))
   (Hypothesis-implies Env Hypothesis Predicate)
   --------------- "prove-Hypothesis"
   (prove Env Predicate)
   ]

  [(prove Env Goal) ...
   --------------- "prove-all"
   (prove Env (All (Goal ...)))
   ]

  [(prove Env Goal_1)
   --------------- "prove-any"
   (prove Env (Any (Goal_0 ... Goal_1 Goal_2 ...)))
   ]

  [(prove (Clauses (Hypothesis_0 ... Hypothesis_1 ...)) Goal)
   --------------- "prove-implies"
   (prove (Clauses (Hypothesis_0 ...)) (Implies (Hypothesis_1 ...) Goal))
   ]

  [; FIXME: universes, etc
   (where/error Substitution (substitution-to-fresh-vars Env Goal KindedVarIds))
   (where/error Goal_1 (apply-substitution Substitution Goal))
   (prove Env Goal_1)
   --------------- "prove-forall"
   (prove Env (ForAll KindedVarIds Goal))
   ]

  [; FIXME: unifiers
   (prove Env (All KindedVarIds Goal))
   --------------- "prove-exists"
   (prove Env (Exists KindedVarIds Goal))
   ]

  )

(define-judgment-form patina-ty
  #:mode (Clause-proves I I I)
  #:contract (Clause-proves Env Clause Predicate)

  [--------------- "clause-fact"
   (Clause-proves Env Predicate Predicate)
   ]

  [(prove Env (all Goals))
   --------------- "clause-backchain"
   (Clause-proves Env (implies Goals Predicate) Predicate)
   ]

  [; FIXME: unifiers
   (where/error Substitution (substitution-to-fresh-vars (Env Clause Predicate) KindedVarIds))
   (where/error Clause_1 (apply-substitution Substitution Clause))
   (Clause-proves Env Clause_1 Predicate)
   --------------- "clause-forall"
   (Clause-proves Env (forall KindedVarIds Clause) Predicate)
   ]

  )

(define-judgment-form patina-ty
  #:mode (Hypothesis-implies I I I)
  #:contract (Hypothesis-implies Env Hypothesis Goal)

  [--------------- "hypothesized"
   (Hypothesis-implies Env Predicate Predicate)
   ]

  [(where (_ ... Hypothesis _ ...) (env-hypotheses Env))
   (Hypothesis-implies Env Hypothesis Predicate_0)
   --------------- "hypothesized-backchain"
   (Hypothesis-implies Env (implies Predicate_0 Predicate_1) Predicate_1)
   ]

  [; FIXME: unifiers
   (where/error Substitution (substitution-to-fresh-vars (Env Hypothesis Predicate) KindedVarIds))
   (where/error Hypothesis_1 (apply-substitution Substitution Hypothesis))
   (Hypothesis-implies Env Hypothesis_1 Predicate)
   --------------- "hypothesized-forall"
   (Hypothesis-implies Env (forall KindedVarIds Hypothesis) Predicate)
   ]
  )

(module+ test
  (test-equal (judgment-holds (prove (() ()) (All ())))
              #t
              )
  )