#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide (all-defined-out))

(define-metafunction patina-ty
  ; Given a set of kinded-var-ids, creates a substituion map that maps them to
  ; fresh names.
  substitution-to-fresh-vars : (any ...) kinded-var-ids -> substitution

  [(substitution-to-fresh-vars (any ...) ((var-kind var-id) ...))
   ((var-id parameter) ...)
   (where/error (var-id_fresh ...) ,(variables-not-in (term (any ...)) (term (var-id ...))))
   (where/error (parameter ...) ((var-kind var-id_fresh) ...))
   ]
  )

(define-metafunction patina-ty
  ; substitute substitution-map any ==> applies a substitution map to anything
  apply-substitution : substitution any -> any

  [(apply-substitution substitution any)
   (substitute-env any substitution)]
  )