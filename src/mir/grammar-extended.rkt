#lang racket
(require redex
         "grammar.rkt")
(provide formality-mir-extended
         )

(define-extended-language formality-mir-extended formality-mir
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  (Γ ::= (CrateDecls KindedVarIds (Tys -> Ty where WhereClauses) MirBody))

  (MaybeVariantId ::= () (VariantId))

  (Location ::= (BasicBlockId @ number))

  ;;
  (StatementAtLocations ::= (StatementAtLocation ...))
  (StatementAtLocation ::= (Location Statement))
  (TerminatorAtLocation ::= (Location Terminator))

  (GoalAtLocations ::= (GoalAtLocation ...))
  (GoalAtLocation ::= (Location Goal))
  )

(define-metafunction formality-mir-extended
  ;; Returns the `AdtContents` of the ADT with the given `AdtId`.
  decl-of-adt : Γ AdtId -> AdtContents

  [(decl-of-adt Γ AdtId)
   AdtContents
   (where/error (_ CrateDecls) Γ)
   (where AdtContents (item-with-id CrateDecls AdtId))
   ]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `MirBody` from the environment Γ
  mir-body-of-Γ : Γ -> MirBody

  [(mir-body-of-Γ (_ _ _ MirBody)) MirBody]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  local-decls-of-Γ : Γ -> LocalDecls

  [(local-decls-of-Γ Γ)
   LocalDecls
   (where/error (LocalDecls _) (mir-body-of-Γ Γ))]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  basic-block-decls-of-Γ : Γ -> BasicBlockDecls

  [(basic-block-decls-of-Γ Γ)
   BasicBlockDecls
   (where/error (_ BasicBlockDecls) (mir-body-of-Γ Γ))]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `CrateDecls` from the environment Γ
  crate-decls-of-Γ : Γ -> MirBody

  [(crate-decls-of-Γ (CrateDecls _ _ _)) CrateDecls]
  )