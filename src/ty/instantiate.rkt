#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide )

(define-metafunction patina-ty
  ; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ; fresh names.
  instantiate-quantified : Env QuantifierKind KindedVarIds any_in -> (Env any_out)

  [(instantiate-quantified Env_0 ForAll KindedVarIds any_in)
   (Env_out any_out)

   (where/error Substitution (substitution-to-fresh-vars (Env_0 any_in) KindedVarIds))
   (where/error ((VarId_old VarId_new) ...) Substitution)
   (where/error (Env_1 Universe) (env-with-next-universe Env_0))
   (where/error Env_2 (env-with-fresh-binding (VarId_new ForAll Universe) ...))
   (where/error any_out (apply-substitution Substitution any_in))
   ]

  [(instantiate-quantified Env_0 Exists KindedVarIds any_in)
   (Env_out any_out)

   (where/error Substitution (substitution-to-fresh-vars (Env_0 any_in) KindedVarIds))
   (where/error ((VarId_old VarId_new) ...) Substitution)
   (where/error Universe (env-universe Env_0))
   (where/error Env_1 (env-with-fresh-binding (VarId_new Exists Universe) ...))
   (where/error any_out (apply-substitution Substitution any_in))
   ]
  )

(module+ test
  (test-equal (term (apply-substitution
                     ((x x1) (y y1))
                     (x (ForAll (TyVar x) (x y) y))))
              (term (x1 (ForAll (TyVar x1) (x1 y1) y1))))
  )