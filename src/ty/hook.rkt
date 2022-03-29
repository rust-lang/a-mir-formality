#lang racket
(require redex/reduction-semantics "grammar.rkt" "../logic/hook.rkt")
(provide formality-ty-hook
         env-adt-variances
         )

;; Extends the core logic hook with the ability to query variance:
;;
;; adt-variance : AdtId -> (RelationOp ...)
(struct formality-ty-hook formality-logic-hook (adt-variance))

(define-metafunction formality-ty
  ;; Returns the variance for each of the parameters to an Adt
  env-adt-variances : Env AdtId -> (RelationOp ...)

  [(env-adt-variances Env AdtId)
   ,(formality-ty-hook-adt-variance (term any))
   (where/error (Hook: any) (env-hook Env))
   ]
  )
