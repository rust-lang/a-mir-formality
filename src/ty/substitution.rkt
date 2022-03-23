#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide substitution-to-fresh-vars
         apply-substitution
         apply-substitution-to-env
         substitution-fix
         substitution-concat-disjoint
         substitution-without-vars
         )

(define-metafunction formality-ty
  ;; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ;; fresh names.
  substitution-to-fresh-vars : Term KindedVarIds -> Substitution

  [(substitution-to-fresh-vars Term ((ParameterKind VarId) ...))
   ((VarId VarId_fresh) ...)
   (where/error (VarId_fresh ...) ,(variables-not-in (term Term) (term (VarId ...))))
   ]
  )

(define-metafunction formality-ty
  ;; Concatenates various substitutions that have disjoint domains.
  substitution-concat-disjoint : Substitution ... -> Substitution

  [(substitution-concat-disjoint Substitution ...)
   ,(apply append (term (Substitution ...)))]
  )

(define-metafunction formality-ty
  ;; Substitute substitution-map any ==> applies a substitution map to anything
  apply-substitution : Substitution Term -> Term

  [(apply-substitution () Term) Term]
  [(apply-substitution ((VarId_0 Term_0) (VarId_1 Term_1) ...) Term_term)
   (apply-substitution ((VarId_1 Term_1) ...) (substitute Term_term VarId_0 Term_0))]
  )

(define-metafunction formality-ty
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

(define-metafunction formality-ty
  ;; Applies the substitution to *itself* until a fixed point is reached.
  substitution-fix : Substitution -> Substitution

  [; if the result of applying substitution to itself has no change, all done
   (substitution-fix Substitution_0)
   Substitution_0
   (where ((VarId_0 Parameter_0) ...) Substitution_0)
   (where (Parameter_0 ...) (apply-substitution Substitution_0 (Parameter_0 ...)))
   ]

  [; otherwise recursively apply
   (substitution-fix Substitution_0)
   (substitution-fix ((VarId_0 Parameter_1) ...))
   (where ((VarId_0 Parameter_0) ...) Substitution_0)
   (where (Parameter_1 ...) (apply-substitution Substitution_0 (Parameter_0 ...)))
   ]
  )

(define-metafunction formality-ty
  ;; Applies the substitution to an environment (not all parts of the
  ;; environment ought to be substituted).
  apply-substitution-to-env : Substitution Env  -> Env

  [(apply-substitution-to-env Substitution (Hook Universe VarBinders Hypotheses))
   (Hook Universe VarBinders (apply-substitution Substitution Hypotheses))]

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