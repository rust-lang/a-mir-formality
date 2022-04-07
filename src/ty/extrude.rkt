#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "predicate.rkt"
         "inequalities.rkt"
         "where-clauses.rkt"
         "parameters.rkt"
         "../logic/substitution.rkt"
         "../logic/env.rkt"
         )

(define-metafunction formality-ty
  ; Creates a new parameter `Parameter_out` of `Parameter` where
  ;
  ; * `Parameter_out InequalityOp Parameter`, assuming that `Goals_out` are
  ; * `Parameter_out` references only names found in `Universe`

  extrude-parameter : Env_in VarIdPairs_in Universe InequalityOp Parameter -> (Env Parameter_out Goals_out)
  #:pre (substitution-valid? Env_in VarIdPairs_in)

  ;; variables

  [; detect cyclic types and re-use the new variable
   ;
   ; always take the *first* match for `VarId_1`
   (extrude-parameter Env (_ ... (VarId VarId_1) _ ...) Universe InequalityOp VarId)
   (Env VarId_1 ())
   ]

  [; variables visible from Universe are just mapped to themselves
   (extrude-parameter Env VarIdPairs Universe InequalityOp VarId)
   (Env VarId ())

   (where/error Universe_VarId (universe-of-var-in-env Env VarId))
   (where #t (universe-includes Universe Universe_VarId))
   ]

  [; existential variable `VarId` not visible from `Universe`: have to map bounds
   ;
   ; We create a new existential variable `VarId_out` in `Universe`.
   ;
   ; * For each bound `Parameter_b` where `Parameter_b InequalityOp VarId`,
   ; we want that `extrude(Parameter_b) InequalityOp VarId_out`.
   ;
   ; * We also want that `VarId_out InequalityOp VarId`, but we can't add that
   ; directly, since `VarId_out` is in a universe that cannot name `VarId`.
   ; Therefore we add an edge `VarId (invert-inequality-op InequalityOp) Parameter_out`.
   (extrude-parameter Env VarIdPairs Universe InequalityOp VarId)
   (Env VarId_out XXX)

   (where/error (Exists Universe_VarId) (var-binding-in-env Env VarId))
   (where/error #f (universe-includes Universe Universe_VarId))
   (where/error (Parameter_b ...) (known-bounds Env InequalityOp VarId))
   (where/error VarId_out (env-))
   ]

  [; universal variable not visible from Universe: have to map bounds
   (extrude-parameter Env VarIdPairs Universe InequalityOp VarId)
   (Env VarId XXX)

   (where/error (ForAll Universe_VarId) (var-binding-in-env Env VarId))
   (where/error #f (universe-includes Universe Universe_VarId))
   (where/error (Parameters_lb Parameters_ub) ())
   ]

  ;; structural recursion

  [(extrude-parameter Env VarIdPairs Universe InequalityOp (TyRigid RigidName Parameters))
   (Env_out (TyRigid RigidName Parameters_out) Goals_out)

   (where/error (Env_out Parameters_out Goals_out) (extrude-parameters Env VarIdPairs Universe InequalityOp Parameters))
   ]

  [(extrude-parameter Env VarIdPairs Universe InequalityOp (ForAll KindedVarIds Ty))
   (Env_out (ForAll KindedVarIds Ty_out) Goals_out)

   ; create a new mapping that has `VarId=VarId` for each newly introduced name, plus
   ; any mappings from `KindedVarIds` that are *not* shadowed
   (where/error ((VarKind VarId) ...) KindedVarIds)
   (where/error (VarIdPair_1 ...) (substitution-without-vars VarIdPairs (VarId ...)))
   (where/error VarIdPairs_new ((VarId VarId) ... VarIdPair_1 ...))

   ; recurse on `Ty` with that new mapping
   (where/error (Env_out Ty_out Goals_out) (extrude-parameter Env VarIdPairs_new Universe InequalityOp Ty))
   ]

  [(extrude-parameter Env VarIdPairs Universe InequalityOp (Implies WhereClauses Ty))
   (Env_out (Implies WhereClauses Ty_out) Goals_out)

   (where/error (Env_out Ty_out Goals_out) (extrude-parameter Env VarIdPairs Universe InequalityOp Ty))
   ]

  )

(define-metafunction formality-ty
  extrude-parameters : Env VarIdPairs Universe InequalityOp Parameters -> (Env Parameters_out Goals_out)

  [(extrude-parameters Env VarIdPairs Universe InequalityOp (Parameter_0 Parameter_1 ...))
   (Env_1 (Parameter_out0 Parameter_out1 ...) (Goal_out0 ... Goal_out1 ...))

   (where/error (Env_0 Parameter_out0 (Goal_out0 ...)) (extrude-parameter Env VarIdPairs Universe InequalityOp Parameter_0))
   (where/error (Env_1 (Parameter_out1 ...) (Goal_out1 ...)) (extrude-parameters Env VarIdPairs Universe InequalityOp (Parameter_1 ...)))
   ]
  )
