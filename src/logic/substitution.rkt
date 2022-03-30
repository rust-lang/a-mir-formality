#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "env.rkt"
         )
(provide substitution-to-fresh-vars
         apply-substitution
         apply-substitution-from-env
         apply-substitution-to-env
         env-fix
         substitution-fix
         substitution-concat-disjoint
         substitution-without-vars
         substitution-maps-var
         env-maps-var
         env-with-var-mapped-to
         )

(define-metafunction formality-logic
  ;; Returns an `Env` where `VarId` is mapped to `Parameter` (`VarId` must not yet be mapped)
  env-with-var-mapped-to : Env_in VarId_in Parameter_in -> Env_out
  #:pre (all? (not? (env-maps-var Env_in VarId_in))
              (env-contains-existential-var Env_in VarId_in)
              #;(universe-of-var-in-env Env_in VarId_in)
              )

  [(env-with-var-mapped-to Env VarId Parameter)
   (Hook Universe VarBinders ((VarId Parameter) (VarId_env Parameter_env) ...) VarInequalities Hypotheses)
   (where/error (Hook Universe VarBinders ((VarId_env Parameter_env) ...) VarInequalities Hypotheses) Env)
   ]
  )


(define-metafunction formality-logic
  ;; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ;; fresh names.
  substitution-to-fresh-vars : Term KindedVarIds -> Substitution

  [(substitution-to-fresh-vars Term ((ParameterKind VarId) ...))
   ((VarId VarId_fresh) ...)
   (where/error (VarId_fresh ...) ,(variables-not-in (term Term) (term (VarId ...))))
   ]
  )

(define-metafunction formality-logic
  ;; Concatenates various substitutions that have disjoint domains.
  substitution-concat-disjoint : Substitution ... -> Substitution

  [(substitution-concat-disjoint Substitution ...)
   ,(apply append (term (Substitution ...)))]
  )

(define-metafunction formality-logic
  ;; True if the Env's substitution includes a mapping for VarId.
  env-maps-var : Env VarId -> boolean

  [(env-maps-var Env VarId)
   (substitution-maps-var (env-substitution Env) VarId)
   ]

  )

(define-metafunction formality-logic
  ;; True if the substitution includes `VarId` in its domain
  substitution-maps-var : Substitution VarId -> boolean

  [(substitution-maps-var (_ ... (VarId _) _ ...) VarId) #t]
  [(substitution-maps-var (_ ...) VarId) #f]
  )

(define-metafunction formality-logic
  ;; Substitute substitution-map any ==> applies a substitution map to anything
  apply-substitution : Substitution Term -> Term

  [(apply-substitution () Term) Term]
  [(apply-substitution ((VarId_0 Term_0) (VarId_1 Term_1) ...) Term_term)
   (apply-substitution ((VarId_1 Term_1) ...) (substitute Term_term VarId_0 Term_0))]
  )

(define-metafunction formality-logic
  ;; Substitute the values for any inference variables found in Env to Term.
  apply-substitution-from-env : Env Term -> Term

  [(apply-substitution-from-env Env Term)
   (apply-substitution Substitution Term)
   (where/error Substitution (env-substitution Env))]
  )

(define-metafunction formality-logic
  ;; Substitute substitution-map any ==> applies a substitution map to anything
  substitution-without-vars : Substitution VarIds -> Substitution

  [(substitution-without-vars () VarIds)
   ()
   ]

  [(substitution-without-vars ((VarId_0 Term_0) (VarId_1 Term_1) ...) VarIds)
   (substitution-without-vars ((VarId_1 Term_1) ...) VarIds)
   (where (_ ... VarId_0 _ ...) VarIds)
   ]

  [(substitution-without-vars ((VarId_0 Term_0) (VarId_1 Term_1) ...) VarIds)
   ((VarId_0 Term_0) (VarId_2 Term_2) ...)
   (where/error ((VarId_2 Term_2) ...) (substitution-without-vars ((VarId_1 Term_1) ...) VarIds))
   ]
  )

(define-metafunction formality-logic
  ;; Apply the environment's substitution to itself until a fixed point is reached.
  ;; Return the new environment with this fixed point.
  env-fix : Env -> Env

  [(env-fix Env)
   (apply-substitution-to-env Substitution_fix Env)
   (where Substitution_fix (subsitution-fix (env-substitution Env)))
   ]

  )

(define-metafunction formality-logic
  ;; Applies the substitution to *itself* until a fixed point is reached.
  substitution-fix : Substitution -> Substitution

  [; if the result of applying substitution to itself has no change, all done
   (substitution-fix Substitution_0)
   Substitution_0
   (where Substitution_0 (apply-substitution-to-substitution Substitution_0 Substitution_0))
   ]

  [; otherwise recursively apply
   (substitution-fix Substitution_0)
   (substitution-fix Substitution_1)
   (where/error Substitution_1 (apply-substitution-to-substitution Substitution_0 Substitution_0))
   ]
  )

(define-metafunction formality-logic
  ;; Applies the substitution to an environment (not all parts of the
  ;; environment ought to be substituted).
  apply-substitution-to-env : Substitution Env  -> Env

  [(apply-substitution-to-env Substitution (Hook Universe VarBinders Substitution_env VarInequalities Hypotheses))
   (Hook
    Universe
    VarBinders
    (apply-substitution-to-substitution Substitution Substitution_env)
    (apply-substitution Substitution VarInequalities) ; the domain of the inequality *ought* to be disjoint from domain of substitution
    (apply-substitution Substitution Hypotheses))]

  )

(define-metafunction formality-logic
  ;; Applies the substitution to an environment (not all parts of the
  ;; environment ought to be substituted).
  apply-substitution-to-substitution : Substitution Substitution -> Substitution

  [(apply-substitution-to-substitution Substitution_0 ((VarId_0 Parameter_0) ...))
   ((VarId_0 Parameter_1) ...)
   (where/error (Parameter_1 ...) (apply-substitution Substitution_0 (Parameter_0 ...)))
   ]

  )

(module+ test
  (test-equal (term (apply-substitution
                     ((x x1) (y y1))
                     (x (ForAll (TyKind x) (x y) y))))
              (term (x1 (ForAll (TyKind x1) (x1 y1) y1))))

  (test-equal (term (apply-substitution
                     ((x x1) (y y1))
                     (x (ForAll (TyKind z) (x y z) y))))
              (term (x1 (ForAll (TyKind z) (x1 y1 z) y1))))

  (test-equal (term (substitution-fix
                     ((x (TyRigid SomeType (y)))
                      (y y1)
                      (z x))))
              (term ((x (TyRigid SomeType (y1)))
                     (y y1)
                     (z (TyRigid SomeType (y1))))))

  (test-equal (term (substitution-without-vars
                     ((x x1) (y y1) (z z1))
                     (y)))
              (term ((x x1) (z z1))))
  )