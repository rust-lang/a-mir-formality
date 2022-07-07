#lang racket
(require redex/reduction-semantics
         "grammar.rkt")
(provide (all-defined-out)
         )

(define-extended-language formality-mir-extended formality-mir
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  (Γ ::= (CrateDecls VarIds_∀ (Tys -> Ty where WhereClauses) LocalsAndBlocks))

  ;; MaybeVariantId -- either a variant-id or nothing
  (MaybeVariantId ::= () (VariantId))

  ;; MirBodySig -- all the information we need when type-checking a MIR function body.
  ;;
  ;; Example, given this Rust function:
  ;;
  ;; ```rust
  ;; fn foo<T, U>(data1: T, data2: U) -> (T, U)
  ;; where
  ;;     T: Debug
  ;; {
  ;;     (data1, data2)
  ;; }
  ;; ```
  ;;
  ;; we would get a `MirBodySig` like
  ;;
  ;; ```
  ;;    (∀ ((type T) (type U)) ((T U) -> (ty-tuple (T U)) where ((T: Debug())) mir))
  ;;        -----------------    - -     ----------------        ------------
  ;;        type-parameters   arguments   return type           where-clauses
  ;; ```
  ;;
  (MirBodySig ::= (∀ KindedVarIds (Tys -> Ty where WhereClauses FnBody)))

  ;; Location --- identifies a particular statement or terminator within the MIR
  (Location ::= (BasicBlockId @ number))

  ;; StatementAtLocation, TerminatorAtLocation -- pair of a statement/terminator with its location
  (StatementAtLocations ::= (StatementAtLocation ...))
  (StatementAtLocation ::= (Location Statement))
  (TerminatorAtLocation ::= (Location Terminator))

  ;; GoalAtLocation -- a goal that must hold at a given program location; used by borrow checking
  ;; to implement flow sensitivity
  (GoalAtLocations ::= (GoalAtLocation ...))
  (GoalAtLocation ::= (Location Goal))
  )

(define-metafunction formality-mir-extended
  ;; Returns the `AdtContents` of the ADT with the given `AdtId`.
  decl-of-adt : Γ AdtId -> AdtDecl

  [(decl-of-adt Γ AdtId)
   (adt-with-id (crate-decls-of-Γ CrateDecls) AdtId)
   ]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `MirBody` from the environment Γ
  locals-and-blocks-of-Γ : Γ -> LocalsAndBlocks

  [(locals-and-blocks-of-Γ (_ _ _ LocalsAndBlocks)) LocalsAndBlocks]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  local-decls-of-Γ : Γ -> LocalDecls

  [(local-decls-of-Γ Γ)
   LocalDecls
   (where/error (LocalDecls _) (locals-and-blocks-of-Γ Γ))]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  basic-block-decls-of-Γ : Γ -> BasicBlockDecls

  [(basic-block-decls-of-Γ Γ)
   BasicBlockDecls
   (where/error (_ BasicBlockDecls) (locals-and-blocks-of-Γ Γ))]
  )

(define-metafunction formality-mir-extended
  ;; Returns the `CrateDecls` from the environment Γ
  crate-decls-of-Γ : Γ -> CrateDecls

  [(crate-decls-of-Γ (CrateDecls _ _ _)) CrateDecls]
  )