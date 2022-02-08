#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide (all-defined-out))

(define-metafunction patina-ty
  ; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ; fresh names.
  substitution-to-fresh-vars : (any ...) KindedVarIds -> Substitution

  [(substitution-to-fresh-vars (any ...) ((VarKind VarId) ...))
   ((VarId VarId_fresh) ...)
   (where/error (VarId ...) ,(variables-not-in (term (any ...)) (term (VarId ...))))
   ]
  )

(define-metafunction patina-ty
  ; substitute substitution-map any ==> applies a substitution map to anything
  apply-substitution : Substitution any -> any

  [(apply-substitution () any) any]
  [(apply-substitution ((VarId_0 any_0) (VarId_1 any_1) ...) any_term)
   (apply-substitution ((VarId_1 any_1) ...) (substitute any_term VarId_0 any_0))]
  )

(module+ test
  (test-equal (term (apply-substitution
                     ((x x1) (y y1))
                     (x (ForAll (TyVar x) (x y) y))))
              (term (x1 (ForAll (TyVar x1) (x1 y1) y1))))

  (test-equal (term (apply-substitution
                     ((x x1) (y y1))
                     (x (ForAll (TyVar z) (x y z) y))))
              (term (x1 (ForAll (TyVar z) (x1 y1 z) y1))))
  )