#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../parameters.rkt"
         "../../logic/env.rkt"
         )
(provide relate-var-to-rigid
         relate-rigid-to-rigid
         )

(define-metafunction formality-ty
  relate-var-to-rigid : Env (VarId RelationOp (TyRigid RigidName Parameters)) -> (Env Goals)

  [; X <op> R<...> -- Inequality between a variable and a rigid type
   ;
   ; addressed by instantiating X with `R<P1...Pn>` for fresh P1...Pn and then requiring
   ; `R<P1...Pn> <op> R<...>` with a goal like
   ;
   ; `∃P1...Pn: (X = R<P1...Pn>) ∧ WF(R<P1..Pn>) ∧ (R<P1..Pn> <op> R<...>)`
   ;
   ; Note the requirement to prove `WF(R<P1..Pn>)`. This is necessary because the `T1 ?= T2`
   ; judgments assume that T1, T2 are WF.
   (relate-var-to-rigid Env (VarId RelationOp (TyRigid RigidName (Parameter ...))))
   (Env (Goal))

   (; get the generic parameters `P1..Pn` for the rigid-name `R`
    where/error ((VarId_rigid (ParameterKind _)) ...) (generic-parameters-for Env RigidName))

   (; make fresh names `VarId_p ...` for each parameter; they don't have to be completely fresh,
    ; they just can't appear in `X` or `R<...>`
    where/error (VarId_p ...) (fresh-var-ids (VarId Parameter ...) (VarId_rigid ...)))

   (; create final goal we will have to prove
    where/error Goal (; ∃P1...Pn:
                      ∃ ((ParameterKind VarId_p) ...)
                        (All ((; (X == R<P1...Pn>) ∧
                               VarId == (TyRigid RigidName (VarId_p ...)))
                              (; (R<P1..Pn> <op> R<...>) ∧
                               (TyRigid RigidName (VarId_p ...)) RelationOp (TyRigid RigidName (Parameter ...)))
                              (; WF(R<P1..Pn>)
                               well-formed (type (TyRigid RigidName (VarId_p ...))))
                              ))))

   ]

  )

(define-metafunction formality-ty
  relate-rigid-to-rigid : Env ((TyRigid RigidName Parameters) RelationOp (TyRigid RigidName Parameters)) -> (Env Goals)

  [; Relating two rigid types with the same name: relate their parameters according to the declared variance.
   (relate-rigid-to-rigid Env ((TyRigid RigidName (Parameter_1 ..._1)) RelationOp (TyRigid RigidName (Parameter_2 ..._1))))
   (Env ((Parameter_1 (apply-variance Variance RelationOp) Parameter_2) ...))
   (where/error (Variance ...) (variances-for Env RigidName))
   (where #t (same-length (Variance ...) (Parameter_1 ...))) ; well-formedness violation otherwise
   ]

  )
