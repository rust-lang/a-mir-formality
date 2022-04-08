#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "predicate.rkt"
         "inequalities.rkt"
         "where-clauses.rkt"
         "parameters.rkt"
         "../logic/instantiate.rkt"
         "../logic/substitution.rkt"
         "../logic/env.rkt"
         )
(provide extrude-parameter
         )

(define-metafunction formality-ty
  ; Creates a new parameter `Parameter_out` of `Parameter` where
  ;
  ; * `Parameter_out InequalityOp Parameter`, assuming that `Goals_out` are
  ; * `Parameter_out` references only names found in `Universe`

  extrude-parameter : Env_in Universe InequalityOp Parameter -> (Env Parameter_out Goals_out)

  [(extrude-parameter Env Universe InequalityOp Parameter)
   (extrude-term Env () Universe InequalityOp Parameter)
   ]
  )

(define-metafunction formality-ty
  ; Creates a new parameter `Parameter_out` of `Parameter` where
  ;
  ; * `Parameter_out InequalityOp Parameter`, assuming that `Goals_out` are
  ; * `Parameter_out` references only names found in `Universe`
  ;
  ; Can also be applied to where-clauses and other kinds of things that may appear
  ; inside a parameter. Always returns a term of the same basic sort that was given.

  extrude-term : Env_in VarIdPairs_in Universe InequalityOp Term -> (Env Term_out Goals_out)
  #:pre (substitution-valid? Env_in VarIdPairs_in)

  ;; variables

  [; detect cyclic types and re-use the new variable
   ;
   ; always take the *first* match for `VarId_1`
   (extrude-term Env (_ ... (VarId VarId_1) _ ...) Universe InequalityOp VarId)
   (Env VarId_1 ())
   ]

  [; variables visible from Universe are just mapped to themselves
   (extrude-term Env VarIdPairs Universe InequalityOp VarId)
   (Env VarId ())

   (where/error Universe_VarId (universe-of-var-in-env Env VarId))
   (where #t (universe-includes Universe Universe_VarId))
   ]

  [; existential variable `VarId` not visible from `Universe`: have to map bounds
   ;
   ; We create a new existential variable `VarId_out` in `Universe`.
   ;
   ; * For each bound `Parameter_b` where `Parameter_b InequalityOp VarId`:
   ;   * Extrude `Parameter_b` to a `Parameter_e` where `Parameter_e invert(InequalityOp) Parameter_b`
   ;   * Enforce that `VarId_out invert(InequalityOp) Parameter_e`
   ;
   ; * We also want that `VarId_out InequalityOp VarId`, but we can't add that
   ;   directly, since `VarId_out` is in a universe that cannot name `VarId`.
   ;   Therefore we add an edge `VarId (invert-inequality-op InequalityOp) Parameter_out`.
   ;
   ; Example. Given `extrude <= X` where
   ;
   ; * `X <= b0`
   ; * `X <= b1`
   ;
   ; we will produce `extrude(X)` where
   ;
   ; * `extrude(b0) <= b0`
   ; * `extrude(b1) <= b1`
   ; * `extrude(X) <= extrude(b0)`
   ; * `extrude(X) <= extrude(b1)`
   ; * `extrude(X) <= X`
   ;
   ; for the final edge, we store it as `X >= extrude(X)`
   (extrude-term Env (VarIdPair ...) Universe InequalityOp_<= VarId)
   ;                                          ^^^^^^^^^^^^^^^ name variable "as if" it represents `<=`
   ;                                                          though of course it could also be `>=`
   (Env_4 VarId_out Goals_b)

   (where (ParameterKind Exists Universe_VarId) (var-binding-in-env Env VarId))
   (where/error InequalityOp_>= (invert-inequality-op InequalityOp_<=))
   (where/error #f (universe-includes Universe Universe_VarId))
   ; create output variable `VarId_out` (`extrude(X)` in the example above)
   (where/error (VarId_out) (fresh-var-ids Env (VarId)))
   (where/error Env_1 (env-with-var Env VarId_out ParameterKind Exists Universe))
   (where/error VarIdPairs_new ((VarId VarId_out) VarIdPair ...))
   ; extract bounds `b_0...b_n` where `X <= b_i` for all `i`
   (where/error (Parameter_b ...) (known-bounds Env InequalityOp_>= VarId))
   ; create `extruded(b_i) <= b_i` for each bound `b_i`
   (where/error (Env_2 Parameters_be Goals_b) (extrude-terms Env_1
                                                             VarIdPairs_new
                                                             Universe
                                                             InequalityOp_<=
                                                             (Parameter_b ...)))
   ; add `extrude(X) <= extrude(b_i)` for each `b_i`
   (where/error Env_3 (env-with-var-related-to-parameters Env_2
                                                          VarId_out
                                                          InequalityOp_<=
                                                          Parameters_be))
   ; add `X >= extrude(X)`
   (where/error Env_4 (env-with-var-related-to-parameter Env_3
                                                         VarId
                                                         InequalityOp_>=
                                                         VarId_out))
   ]

  [; universal variable not visible from Universe: create "any" goal
   ;
   ; We create a new existential variable `VarId_out` in `Universe` where `VarId_out <= !X`.
   ;
   ; * For any bound `Parameter_b` where `Parameter_b InequalityOp VarId`:
   ;   * Extrude `Parameter_b` to a `Parameter_e` where `Parameter_e InequalityOp Parameter_b`
   ;   * Enforce that `VarId_out InequalityOp Parameter_e`
   ;
   ; Example. Given `extrude <= !X` where
   ;
   ; * `b0 <= !X`
   ; * `b1 <= !X`
   ;
   ; we will produce `extrude(X)` with
   ;
   ; * `extrude(b0) <= b0`
   ; * `extrude(b1) <= b1`
   ;
   ; and the goal `Any(extrude(X) <= extrude(b0), extrude(X) <= extrude(b1))`
   (extrude-term Env (VarIdPair ...) Universe InequalityOp_<= VarId)
   ;                                          ^^^^^^^^^^^^^^^ name variable "as if" it represents `<=`
   ;                                                          though of course it could also be `>=`
   (Env_2 VarId_out ((Any ((VarId_out InequalityOp_<= Parameter_be) ...)) Goal_b ...))

   (where (ParameterKind ForAll Universe_VarId) (var-binding-in-env Env VarId))
   (where/error #f (universe-includes Universe Universe_VarId))
   ; create output variable `VarId_out` (`extrude(X)` in the example above)
   (where/error (VarId_out) (fresh-var-ids Env (VarId)))
   (where/error Env_1 (env-with-var Env VarId_out ParameterKind Exists Universe))
   (where/error VarIdPairs_new ((VarId VarId_out) VarIdPair ...))
   ; extract bounds `b_0...b_n` where `b_i <= X` for all `i`
   ; FIXME: connect this to hypotheses somehow
   (where/error Parameters_b (known-bounds Env_1 InequalityOp_<= VarId))
   ; create `extruded(b_i) >= b_i` for each bound `b_i`
   (where/error (Env_2 (Parameter_be ...) (Goal_b ...)) (extrude-terms Env_1
                                                                       VarIdPairs_new
                                                                       Universe
                                                                       InequalityOp_<=
                                                                       Parameters_b))
   ]

  ;; structural recursion

  [(extrude-term Env VarIdPairs Universe InequalityOp (TyRigid RigidName Parameters))
   (Env_out (TyRigid RigidName Parameters_out) Goals_out)

   (where/error (Env_out Parameters_out Goals_out) (extrude-terms Env VarIdPairs Universe InequalityOp Parameters))
   ]

  [(extrude-term Env VarIdPairs Universe InequalityOp (ForAll KindedVarIds Term))
   (Env_out (ForAll KindedVarIds Term_out) Goals_out)

   ; create a new mapping that has `VarId=VarId` for each newly introduced name, plus
   ; any mappings from `KindedVarIds` that are *not* shadowed
   (where/error ((VarKind VarId) ...) KindedVarIds)
   (where/error (VarIdPair_1 ...) (substitution-without-vars VarIdPairs (VarId ...)))
   (where/error VarIdPairs_new ((VarId VarId) ... VarIdPair_1 ...))

   ; recurse on `Ty` with that new mapping
   (where/error (Env_out Term_out Goals_out) (extrude-term Env VarIdPairs_new Universe InequalityOp Term))
   ]

  [(extrude-term Env VarIdPairs Universe InequalityOp (Implies WhereClauses Ty))
   (Env_out (Implies WhereClauses Ty_out) (append Goals_ty Goals_wc))

   (where/error (Env_1 Ty_out Goals_ty) (extrude-term Env VarIdPairs Universe InequalityOp Ty))
   (where/error (Env_2 WhereClauses_out Goals_wc) (extrude-terms Env VarIdPairs Universe InequalityOp WhereClauses))
   ]

  [(extrude-term Env VarIdPairs Universe InequalityOp (Implemented (TraitId Parameters)))
   (Env_out (Implemented (TraitId Parameters_out)) Goals_out)

   (where/error (Env_out Parameters_out Goals_out) (extrude-terms Env VarIdPairs Universe InequalityOp Parameters))
   ]
  )

(define-metafunction formality-ty
  extrude-terms : Env VarIdPairs Universe InequalityOp Terms -> (Env Terms_out Goals_out)

  [(extrude-terms Env VarIdPairs Universe InequalityOp ())
   (Env () ())
   ]

  [(extrude-terms Env VarIdPairs Universe InequalityOp (Term_0 Term_1 ...))
   (Env_1 (Term_out0 Term_out1 ...) (Goal_out0 ... Goal_out1 ...))

   (where/error (Env_0 Term_out0 (Goal_out0 ...)) (extrude-term Env VarIdPairs Universe InequalityOp Term_0))
   (where/error (Env_1 (Term_out1 ...) (Goal_out1 ...)) (extrude-terms Env_0 VarIdPairs Universe InequalityOp (Term_1 ...)))
   ]
  )
