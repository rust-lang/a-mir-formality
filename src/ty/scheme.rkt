#lang racket
(require racket/set
         redex/reduction-semantics
         "grammar.rkt"
         "inequalities.rkt"
         "../logic/env.rkt"
         "../logic/substitution.rkt"
         )
(provide extract-scheme
         extract-schemes
         )

(define-metafunction formality-ty
  ;; Extract schemes from multiple environments for the same terms.
  ;; Just here to help with the testing macro.
  extract-schemes : Envs Term -> Schemes

  [(extract-schemes (Env ...) Term)
   ((extract-scheme Env Term) ...)
   ]
  )

(define-metafunction formality-ty
  extract-scheme : Env Term -> Scheme

  [(extract-scheme Env Term)
   (Exists KindedVarIds (Implies Goals Term_subst))
   (where/error Term_subst (apply-substitution-from-env Env Term))
   (where/error ((VarId_free ...) Goals) (extract-goals-for-vars-fix Env () () Term_subst))
   (where/error KindedVarIds ((add-var-kind Env VarId_free) ...))
   ]
  )

(define-metafunction formality-ty
  ;; If `VarId` is an existential, return a 1-element list with `(ParameterKind VarId)`.
  ;; Else return empty list.
  add-var-kind : Env VarId -> KindedVarId

  [(add-var-kind Env VarId)
   (ParameterKind VarId)
   (where/error (ParameterKind Exists _) (var-binding-in-env Env VarId))
   ]

  )

(define-metafunction formality-ty
  ;; Find free variables appearing in either `Goals` or `Term` that are
  ;; not members of `VarIds`; extract any relevant bounds on them
  ;; from the environment and add those to `Goals`. Repeat until fixed point is reached.
  extract-goals-for-vars-fix : Env VarIds Goals Term -> (VarIds Goals)

  [(extract-goals-for-vars-fix Env VarIds Goals Term)
   (VarIds Goals)
   (where/error VarIds_free (free-existential-variables Env (Goals Term)))
   (where () ,(set-subtract (term VarIds_free) (term VarIds)))
   ]

  [(extract-goals-for-vars-fix Env (VarId_in ...) (Goal ...) Term)
   (extract-goals-for-vars-fix Env
                               (VarId_in ... VarId_extra ...)
                               (Goal ... Goal_subst ... ...)
                               Term)
   (where/error VarIds_free (free-existential-variables Env (Goal ... Term)))
   (where (VarId_extra ...) ,(set-subtract (term VarIds_free) (term (VarId_in ...))))
   (where/error ((Goal_extra ...) ...) ((known-relations Env VarId_extra) ...))
   (where/error ((Goal_subst ...) ...) (((apply-substitution-from-env Env Goal_extra) ...) ...))
   ]
  )
