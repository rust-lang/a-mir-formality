#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "substitution.rkt"
         "hook.rkt"
         "env.rkt"
         "../util.rkt")
(provide instantiate-quantified)

(define-metafunction formality-logic
  ; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ; fresh names.
  instantiate-quantified : Env (Quantifier KindedVarIds Term_0) -> (Env Term_out VarIds)

  [; Handle universal binder instantiation.
   ;
   ; In this case, we want to replace e.g. `for<T> Vec<T>` with
   ; `Vec<T1<>>` for some fresh name `T1` -- i.e., `(TyRigid Vec (TyRigid T1 ()))`.
   (instantiate-quantified Env_0 (∀ KindedVarIds Term_0))
   (Env_2 Term_1 (VarId_new ...))

   ; map the `KindedVarIds` to fresh names that do not appear in the environment `Env_0`
   ; or the input term `Term_0`
   (where/error Substitution_to_placeholders (substitution-to-fresh-vars (Env_0 Term_0) KindedVarIds))
   (where/error ((VarId_old VarId_new) ...) Substitution_to_placeholders)

   ; create a new environment where the fresh names are placed in a fresh universe
   (where/error Env_1 (env-with-incremented-universe Env_0))
   (where/error ((ParameterKind _) ...) KindedVarIds)
   (where/error Env_2 (env-with-vars-in-current-universe Env_1 ∀ ((ParameterKind VarId_new) ...)))

   ; substitute the uses of bound variables in the term with their placeholders
   (where/error Term_1 (apply-substitution Substitution_to_placeholders Term_0))
   ]

  [; Handle existential binder instantiation.
   ;
   ; In this case, we want to replace e.g. `exists<T> Vec<T>` with
   ; `Vec<T1>` for some fresh name `T1` -- i.e., `(TyRigid Vec (T1))`.
   (instantiate-quantified Env_0 (Exists KindedVarIds Term_0))
   (Env_1 Term_1 (VarId_new ...))

   ; map the `KindedVarIds` to fresh names that do not appear in the environment `Env_0`
   ; or the input term `Term_0`
   (where/error Substitution_to_inference (substitution-to-fresh-vars (Env_0 Term_0) KindedVarIds))
   (where/error ((VarId_old VarId_new) ...) Substitution_to_inference)

   ; these names will be placed in the current universe of the environment
   (where/error ((ParameterKind _) ...) KindedVarIds)
   (where/error Env_1 (env-with-vars-in-current-universe Env_0 Exists ((ParameterKind VarId_new) ...)))

   ; substitute the uses of bound variables in the term with their inference variables
   (where/error Term_1 (apply-substitution Substitution_to_inference Term_0))
   ]
  )

(module+ test
  (require "test/hook.rkt")

  (redex-let*
   formality-logic
   []

   (test-match-terms
    formality-logic
    (term (instantiate-quantified EmptyEnv (∀ ((Atom V)) (Vec (V)))))
    (term (_ (Vec (VarId_V1)) (VarId_V1))))

   (test-match-terms
    formality-logic
    (term (instantiate-quantified EmptyEnv (Exists ((Atom V)) (Vec (V)))))
    (term (_ (Vec (VarId_V1)) (VarId_V1))))
   )
  )