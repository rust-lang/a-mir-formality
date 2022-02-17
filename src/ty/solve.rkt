#lang racket
(require redex
         "grammar.rkt"
         "substitution.rkt"
         "instantiate.rkt"
         "unify.rkt")
(provide prove)

(define-judgment-form formality-ty
  #:mode (prove I I O)
  #:contract (prove Env Goal EnvSubstitution)

  [(where (_ ... Clause _ ... ) (env-clauses Env))
   (Clause-proves Env Clause Predicate EnvSubstitution_out)
   --------------- "prove-Clause"
   (prove Env Predicate EnvSubstitution_out)
   ]

  [(where (_ ... Hypothesis _ ... ) (env-hypotheses Env))
   (Hypothesis-implies Env Hypothesis Predicate EnvSubstitution_out)
   --------------- "prove-Hypothesis"
   (prove Env Predicate EnvSubstitution_out)
   ]

  [(prove-all Env Goals EnvSubstitution_out)
   --------------- "prove-all"
   (prove Env (All Goals) EnvSubstitution_out)
   ]

  [(prove Env Goal_1 EnvSubstitution_out)
   --------------- "prove-any"
   (prove Env (Any (Goal_0 ... Goal_1 Goal_2 ...)) EnvSubstitution_out)
   ]

  [(where Env_1 (env-with-hypotheses Env Hypotheses))
   (prove Env_1 Goal EnvSubstitution_out)
   --------------- "prove-implies"
   (prove Env (Implies Hypotheses Goal) (reset Env EnvSubstitution_out))
   ]

  [(where/error (Env_1 Goal_1) (instantiate-quantified Env ForAll KindedVarIds Goal))
   (prove Env Goal_1 EnvSubstitution_out)
   --------------- "prove-forall"
   (prove Env (ForAll KindedVarIds Goal) (reset Env EnvSubstitution_out))
   ]

  [(where/error (Env_1 Goal_1) (instantiate-quantified Env Exists KindedVarIds Goal))
   (prove Env Goal_1 EnvSubstitution_out)
   --------------- "prove-exists"
   (prove Env (Exists KindedVarIds Goal) (reset Env EnvSubstitution_out))
   ]

  )

(define-judgment-form formality-ty
  #:mode (prove-all I I O)
  #:contract (prove-all Env Goals EnvSubstitution)

  [----------------
   (prove-all Env () (Env ()))]

  [(prove Env Goal_0 (Env_1 Substitution_1))
   (prove-all Env_1 (apply-substitution Substitution_1 (Goal_1 ...)) EnvSubstitution_out)
   ----------------
   (prove-all Env (Goal_0 Goal_1 ...) (merge-substitution Substitution_1 EnvSubstitution_out))]

  )

(define-judgment-form formality-ty
  #:mode (Clause-proves I I I O)
  #:contract (Clause-proves Env Clause Predicate EnvSubstitution)

  [--------------- "clause-fact"
   (Clause-proves Env Predicate Predicate (Env ()))
   ]

  [(prove Env (all Goals) EnvSubstitution)
   --------------- "clause-backchain"
   (Clause-proves Env (implies Goals Predicate) Predicate EnvSubstitution)
   ]

  [(where/error Substitution (substitution-to-fresh-vars (Env Clause Predicate) KindedVarIds))
   (where/error Clause_1 (apply-substitution Substitution Clause))
   (Clause-proves Env Clause_1 Predicate EnvSubstitution)
   --------------- "clause-forall"
   (Clause-proves Env (forall KindedVarIds Clause) Predicate (reset Env EnvSubstitution))
   ]

  )

(define-judgment-form formality-ty
  #:mode (Hypothesis-implies I I I O)
  #:contract (Hypothesis-implies Env Hypothesis Goal EnvSubstitution_out)

  [--------------- "hypothesized"
   (Hypothesis-implies Env Predicate Predicate (Env ()))
   ]

  [(where (_ ... Hypothesis _ ...) (env-hypotheses Env))
   (Hypothesis-implies Env Hypothesis Predicate_0 EnvSubstitution_out)
   --------------- "hypothesized-backchain"
   (Hypothesis-implies Env (implies Predicate_0 Predicate_1) Predicate_1 EnvSubstitution_out)
   ]

  [(where/error (Env_1 Hypothesis_1) (instantiate-quantified Env ForAll KindedVarIds Hypothesis))
   (Hypothesis-implies Env_1 Hypothesis_1 Predicate EnvSubstitution_out)
   --------------- "hypothesized-forall"
   (Hypothesis-implies Env (ForAll KindedVarIds Hypothesis) Predicate (reset Env EnvSubstitution_out))
   ]
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  reset : Env EnvSubstitution -> EnvSubstitution

  [(reset Env (Env_out Substitution_out))
   ((reset-env Env Env_out) (reset-substitution Env Substitution_out))]
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  reset-env : Env_old Env_new -> Env

  [(reset-env (Universe ((VarId Universe_old) ...) Clauses Hypotheses) (_ VarUniverses_new _ _))
   (Universe ((VarId Universe_new) ...) Clauses Hypotheses)
   (where/error ((VarId_new _) ... (VarId Universe_new) ...) VarUniverses_new)
   ]
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  reset-substitution : Env Substitution -> Substitution

  [(reset-substitution Env ()) ()]

  [(reset-substitution Env ((VarId Parameter) VarParameter_in ...))
   ((VarId Parameter) VarParameter_out ...)
   (where #t (var-defined-in-env Env VarId))
   (where/error (VarParameter_out ...) (reset-substitution Env (VarParameter_in ...)))]

  [(reset-substitution Env ((VarId Parameter) VarParameter_in ...))
   (reset-substitution Env (VarParameter_in ...))
   (where #f (var-defined-in-env Env VarId))]

  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  merge-substitution : Substitution EnvSubstitution -> EnvSubstitution

  [(merge-substitution Substitution_0 (Env Substitution_1))
   (Env (substitution-concat-disjoint Substitution_0 Substitution_1))]
  )

(module+ test
  (redex-let*
   formality-ty
   ((; A is in U0
     (Env_0 Ty_A) (term (instantiate-quantified EmptyEnv Exists ((TyVar A)) A)))
    (; V is a placeholder in U1
     (Env_1 Ty_T) (term (instantiate-quantified Env_0 ForAll ((TyVar T)) T)))
    (; X is in U1
     (Env_2 Ty_X) (term (instantiate-quantified Env_1 Exists ((TyVar X)) X)))
    (; Y, Z are in U1
     (Env_3 (Ty_Y Ty_Z)) (term (instantiate-quantified Env_2 Exists ((TyVar Y) (TyVar Z)) (Y Z))))
    ((Env_4 Substitution_out) (term (most-general-unifier Env_3 (((TyApply Vec (Ty_A)) (TyApply Vec (Ty_X)))))))
    (Env_5 (term (reset-env Env_2 Env_4)))
    )

   (test-equal (term (reset-env Env_0 Env_1)) (term Env_0))
   (test-equal (term (reset-env Env_0 Env_2)) (term Env_0))

   ;; check that X was originally in U1 but was reduced to U0,
   ;; and that reduction survived the "reset-env" call
   (test-equal (term (universe-of-var-in-env Env_2 Ty_X)) (term (next-universe RootUniverse)))
   (test-equal (term (universe-of-var-in-env Env_4 Ty_X)) (term RootUniverse))
   (test-equal (term (universe-of-var-in-env Env_5 Ty_X)) (term RootUniverse))

   (test-equal
    (judgment-holds (prove EmptyEnv (All ()) EnvSubstitution) EnvSubstitution)
    (term ((EmptyEnv ()))))

       (test-equal
    (judgment-holds (prove EmptyEnv (All ()) EnvSubstitution) EnvSubstitution)
    (term ((EmptyEnv ()))))
   ))
