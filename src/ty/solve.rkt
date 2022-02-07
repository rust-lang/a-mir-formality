#lang racket
(require redex "grammar.rkt" "substitution.rkt")
(provide prove)

(define-judgment-form patina-ty
  #:mode (prove I I)
  #:contract (prove Env Goal)

  [(where ((_ ... ProgramClause _ ... ) _) Env)
   (ProgramClause-proves Env ProgramClause Predicate)
   --------------- "prove-ProgramClause"
   (prove Env Predicate)
   ]

  [(where (_ (_ ... Hypothesis _ ... )) Env)
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

  [(prove (ProgramClauses (Hypothesis_0 ... Hypothesis_1 ...)) Goal)
   --------------- "prove-implies"
   (prove (ProgramClauses (Hypothesis_0 ...)) (Implies (Hypothesis_1 ...) Goal))
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
  #:mode (ProgramClause-proves I I I)
  #:contract (ProgramClause-proves Env ProgramClause Predicate)

  [--------------- "pc-fact"
   (ProgramClause-proves Env Predicate Predicate)
   ]

  [(prove Env (all Goals))
   --------------- "pc-backchain"
   (ProgramClause-proves Env (implies Goals Predicate) Predicate)
   ]

  [; FIXME: unifiers
   (where/error Substitution (substitution-to-fresh-vars (Env ProgramClause Predicate) KindedVarIds))
   (where/error ProgramClause_1 (apply-substitution Substitution ProgramClause))
   (ProgramClause-proves Env ProgramClause_1 Predicate)
   --------------- "pc-forall"
   (ProgramClause-proves Env (forall KindedVarIds ProgramClause) Predicate)
   ]

  )

(define-judgment-form patina-ty
  #:mode (Hypothesis-implies I I I)
  #:contract (Hypothesis-implies Env Hypothesis Goal)

  [--------------- "hypothesized"
   (Hypothesis-implies Env Predicate Predicate)
   ]

  [(where (_ (_ ... Hypothesis _ ...)) Env)
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