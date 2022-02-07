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

  [(apply-substitution Substitution any)
   (substitute-env any Substitution)]
  )
