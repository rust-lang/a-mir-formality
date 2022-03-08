#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "substitution.rkt"
         "../util.rkt")
(provide instantiate-quantified)

(define-metafunction formality-ty
  ; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ; fresh names.
  instantiate-quantified : Env Quantifier KindedVarIds Term_0 -> (Env Term_out VarIds)

  [; Handle universal binder instantiation.
   ;
   ; In this case, we want to replace e.g. `for<T> Vec<T>` with
   ; `Vec<T1<>>` for some fresh name `T1` -- i.e., `(TyApply Vec (TyApply T1 ()))`.
   (instantiate-quantified Env_0 ForAll KindedVarIds Term_0)
   (Env_2 Term_1 (VarId_new ...))

   ; map the `KindedVarIds` to fresh names that do not appear in the environment `Env_0`
   ; or the input term `Term_0`
   (where/error ((VarId_old VarId_new) ...) (substitution-to-fresh-vars (Env_0 Term_0) KindedVarIds))

   ; create a new environment where the fresh names are placed in a fresh universe
   (where/error Env_1 (env-with-incremented-universe Env_0))
   (where/error Env_2 (env-with-vars-in-current-universe Env_1 ForAll (VarId_new ...)))

   ; map each new variable to a Parameter that uses it in placeholder role
   (where/error Substitution_to_placeholders ((VarId_old (! VarId_new)) ...))

   ; substitute the uses of bound variables in the term with their placeholders
   (where/error Term_1 (apply-substitution Substitution_to_placeholders Term_0))
   ]

  [; Handle existential binder instantiation.
   ;
   ; In this case, we want to replace e.g. `exists<T> Vec<T>` with
   ; `Vec<T1>` for some fresh name `T1` -- i.e., `(TyApply Vec (T1))`.
   (instantiate-quantified Env_0 Exists KindedVarIds Term_0)
   (Env_1 Term_1 (VarId_new ...))

   ; map the `KindedVarIds` to fresh names that do not appear in the environment `Env_0`
   ; or the input term `Term_0`
   (where/error Substitution_to_inference (substitution-to-fresh-vars (Env_0 Term_0) KindedVarIds))

   ; map `X => (? X)`
   (where/error ((VarId_old VarId_new) ...) Substitution_to_inference)

   ; these names will be placed in the current universe of the environment
   (where/error Env_1 (env-with-vars-in-current-universe Env_0 Exists (VarId_new ...)))
   (where/error Term_1 (apply-substitution Substitution_to_inference Term_0))
   ]
  )

(define-metafunction formality-ty
  ;; Helper function for instantiating `ForAll` binders:
  ;;
  ;; Given a binder like `ForAll<type T, lifetime L>`, this function is
  ;; invoked with a substitution `T => T1, L => L1` mapping `T` and `L`
  ;; to fresh names, along with some element of the original binder
  ;; like `type T` or `lifetime L`. It returns a mapping `T => (TyApply T1 ())`
  ;; that maps from the original variable to a parameter using the new
  ;; name in placeholder position.
  placeholder-parameter : Substitution KindedVarId -> (VarId Parameter)

  [(placeholder-parameter Substitution (TyKind VarId))
   (VarId (TyApply VarId_new ()))
   (where VarId_new (apply-substitution Substitution VarId))]

  [(placeholder-parameter Substitution (LifetimeVar VarId))
   (VarId (LtApply VarId_new ()))
   (where VarId_new (apply-substitution Substitution VarId))]
  )

(module+ test

  (redex-let*
   formality-ty
   []

   (test-match-terms
    formality-ty
    (term (instantiate-quantified EmptyEnv ForAll ((TyKind V)) (TyApply Vec (V))))
    (term (_ (TyApply Vec ((! V1))) (V1))))

   (test-match-terms
    formality-ty
    (term (instantiate-quantified EmptyEnv Exists ((TyKind V)) (TyApply Vec (V))))
    (term (_ (TyApply Vec (V1)) (V1))))
   )
  )