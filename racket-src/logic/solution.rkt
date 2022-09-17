#lang racket
(require racket/set
         redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         "../logic/env-inequalities.rkt"
         "../logic/substitution.rkt"
         )
(provide extract-solution
         )

;; NB: Unit tests for this file live in racket-src/ty/test/test-solution.
;; This lets us use the type layer to

(define-metafunction formality-logic
  ;; Given the final environment `Env` that resulted from solving `QueryGoal`,
  ;; extracts the solution.

  extract-solution : Env VarIds -> Solution

  [(extract-solution Env VarIds_in)
   (:- ∃VarBinders (Substitution_in Relations))

   ; Find the existential variables that appeared in the original term
   ; and, for those that have a fixed value now, created a substitution to that value.
   (where/error [VarId_in ...] VarIds_in)
   (where/error Substitution_in (substitution-concat-disjoint (variable-substitution Env VarId_in) ...))

   ; Looking at the final values of `VarIds_in`, extract the list of existential variables
   ; that still appear (some of which we may have instantiated!) and the relationships between them.
   ; This is a fixed point process because we may have an initial set of variables like
   ;
   ;     (?T ?c)
   ;
   ; which, after we apply the substitution, becomes:
   ;
   ;        ((&?a u32) ?c)
   ;
   ; and where we have the following relations in the environment:
   ;
   ;     ?a: ?b
   ;     ?b: ?c
   ;
   ; The `extract-goals-for-vars-fix` will expand the set of relevant variables
   ; to include `?a` and then, because of the `?a: ?b` edge, `?b`.
   (where/error Term_subst (apply-substitution-from-env Env VarIds_in))
   (where/error (VarIds_free Relations) (extract-goals-for-vars-fix Env () () Term_subst))

   (where [VarId_new ...] ,(set-subtract (term VarIds_free) (term VarIds_in)))
   (where/error ∃VarBinders ((var-binding-in-env Env VarId_new) ...))
   ]
  )

(define-metafunction formality-logic
  ;; Given an environment and an existential variable `VarId`, returns...
  ;;
  ;; * an empty substitution, in the case that VarId is not mapped in Env
  ;; * a substitution `[X -> P]` in the case that `Env` maps `X` to the parameter `P`.
  variable-substitution : Env VarId -> Substitution

  [(variable-substitution Env VarId)
   []
   (where VarId (apply-substitution-from-env Env VarId))]

  [(variable-substitution Env VarId)
   [(VarId Parameter)]
   (where Parameter (apply-substitution-from-env Env VarId))]

  )

(define-metafunction formality-logic
  ;; Find free variables appearing in either `Goals` or `Term` that are
  ;; not members of `VarIds`; extract any relevant bounds on them
  ;; from the environment and add those to `Goals`. Repeat until fixed point is reached.
  extract-goals-for-vars-fix : Env VarIds Relations Term -> (VarIds Relations)

  [(extract-goals-for-vars-fix Env VarIds Relations Term)
   (VarIds Relations)
   (where/error VarIds_free (free-existential-variables Env (Relations Term)))
   (where () ,(set-subtract (term VarIds_free) (term VarIds)))
   ]

  [(extract-goals-for-vars-fix Env (VarId_in ...) (Relation_in ...) Term)
   (extract-goals-for-vars-fix Env
                               (VarId_in ... VarId_extra ...)
                               (Relation_in ... Relation_subst ... ...)
                               Term)
   (where/error VarIds_free (free-existential-variables Env (Relation_in ... Term)))
   (where (VarId_extra ...) ,(set-subtract (term VarIds_free) (term (VarId_in ...))))
   (where/error ((Relation_extra ...) ...) ((known-relations Env VarId_extra) ...))
   (where/error ((Relation_subst ...) ...) (((apply-substitution-from-env Env Relation_extra) ...) ...))
   ]
  )
