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
  ;; Used to extract the final answer after a goal has been solved.
  ;; You invoke it with the final environment (`Env`) and the original goal (`Term`).
  ;; The resulting `Scheme` will include the goal rephrased with anything we could infer,
  ;; but this "rephrased" goal may include existential variables and relations between them.
  ;;
  ;; Example input goal
  ;;
  ;;    ?0 == Vec<?1>
  ;;
  ;; Output
  ;;
  ;;

  extract-scheme : Env Term -> Scheme

  [(extract-scheme Env Term)
   (∃ KindedVarIds (implies Relations Term_subst))
   (where/error Term_subst (apply-substitution-from-env Env Term))
   (where/error ((VarId_free ...) Relations) (extract-goals-for-vars-fix Env () () Term_subst))
   (where/error KindedVarIds ((add-var-kind Env VarId_free) ...))
   ]
  )

(define-metafunction formality-ty
  ;; If `VarId` is an existential, return a 1-element list with `(ParameterKind VarId)`.
  ;; Else return empty list.
  add-var-kind : Env VarId -> KindedVarId

  [(add-var-kind Env VarId)
   (ParameterKind VarId)
   (where/error (_ ParameterKind ∃ _) (var-binding-in-env Env VarId))
   ]

  )

(define-metafunction formality-ty
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
