#lang racket
(require redex/reduction-semantics
         "../decl/grammar.rkt"
         )
(provide (all-defined-out))

(define-extended-language formality-body formality-decl
  ; Overridden from formality-decl
  (FnBody ::=
          (∃ KindedVarIds LocalsAndBlocks)
          {trusted-fn-body} ; a special tag that is always considered to pass the type check
          )
  (LocalsAndBlocks ::= (LocalDecls BasicBlockDecls))

  ;; A `LocalDecl` indicates the type of a local variable.
  (LocalDecls ::= (LocalDecl ...))
  (LocalDecl ::= (LocalId Ty MaybeMut))

  ;; A `BasicBlockDecl` declares the data for a basic block: its statements, terminator, etc.
  (BasicBlockDecls ::= (BasicBlockDecl ...))
  (BasicBlockDecl ::= (BasicBlockId BasicBlockData))
  (BasicBlockData ::= (Statements Terminator))

  ;; A MIR statement is a single executiable unit within a basic block.
  (Statements ::= (Statement ...))
  (Statement ::=
             (Place = Rvalue)
             (set-discriminant Place VariantId)
             (storage-live LocalId)
             (storage-dead LocalId)
             noop
             (fake-read Place)
             )

  ;; A Rvalue indicates the set of expressions that can be evaluated into a place.
  (Rvalue ::=
          (use Operand)
          (repeat Operand Constant)
          (ref Lt MaybeMut Place)
          (addr-of MaybeMut Place)
          (len Place)
          (BinaryOp Operand Operand)
          (AggregateKind Operands)
          (cast Operand as Ty)
          unknown-rvalue
          )

  (BinaryOp ::=
            BinaryMathOp
            BinaryComparisonOp)
  (BinaryMathOp ::= BinaryMathOpUnchecked (checked BinaryMathOpUnchecked))
  (BinaryMathOpUnchecked ::= + - * /)
  (BinaryComparisonOp ::= < <= > >=)

  (AggregateKind ::=
                 tuple
                 (adt AdtId VariantId Parameters)
                 )

  ;; A `Terminator` ends a basic block and branches to other blocks.
  (Terminator ::=
              (goto BasicBlockId)
              resume
              abort
              return
              unreachable
              (drop Place TargetIds)
              (drop-and-replace Place Operand TargetIds)
              (call Operand Operands Place TargetIds)
              (assert Operand ; operand (must be bool)
                      boolean ; should equal this value
                      TargetIds ; if true, branch to first target; if false, panic and branch to second target
                      )
              (switch-int Operand ; operand to test, which must be integral
                          Ty ; expected type of the operand (can be any scalar type)
                          SwitchTargets ; values to jump to
                          OtherwiseTarget ; place to jump if nothingin the list matches; if empty, exhaustive
                          )
              )
  (TargetIds ::=
             (BasicBlockId) ; unwind not possible
             (BasicBlockId BasicBlockId) ; unwind possible
             )
  (SwitchTargets ::= [SwitchTarget ...])
  (SwitchTarget (number BasicBlockId))
  (OtherwiseTarget ::= [] [BasicBlockId])

  ;; An `Operand` is the argument to an rvalue.
  (Operands ::= [Operand ...])
  (Operand ::=
           (CopyMove Place)
           (const Constant)
           )

  (CopyMove ::= copy move)

  (Constant ::=
            number
            true
            false
            (fn-ptr FnId Parameters)
            (static StaticId)
            (tuple (Constant ...))
            )

  (Places ::= [Place ...])
  (Place ::= LocalId CompoundPlace)
  (CompoundPlace ::=
                 (* Place)
                 (field Place FieldName)
                 (index Place LocalId)
                 (downcast Place VariantId)
                 )

  (Projections ::= (Projection ...))
  (Projection ::=
              *
              (field FieldName)
              (index LocalId)
              (downcast VariantId)
              )

  ;; Internal to type check:
  ;;
  ;; Extension of `Ty` to optionally store a `VariantId`.
  (PlaceTy ::= (place-ty Ty MaybeMut) (place-ty-variant Ty MaybeMut VariantId))

  ;; Internal to type check:
  ;;
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  (Γ ::= (CrateDecls VarIds_∀ (Tys -> Ty where Biformulas) VarIds_∃ LocalsAndBlocks))

  ;; Internal to type check:
  ;;
  ;; MaybeVariantId -- either a variant-id or nothing
  (MaybeVariantId ::= () (VariantId))

  ;; Internal to type check:
  ;;
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
  (MirBodySig ::= (∀ KindedVarIds (Tys -> Ty where Biformulas FnBody)))

  ;; Internal to type check:
  ;;
  ;; Location --- identifies a particular statement or terminator within the MIR
  (Locations ::= [Location ...])
  (Location ::= (BasicBlockId @ number))

  ;; Internal to type check:
  ;;
  ;; StatementAtLocation, TerminatorAtLocation -- pair of a statement/terminator with its location
  (StatementAtLocations ::= (StatementAtLocation ...))
  (StatementAtLocation ::= (Location Statement))
  (TerminatorAtLocation ::= (Location Terminator))
  (StatementOrTerminator ::= Statement Terminator)

  ;; Internal to type check:
  ;;
  ;; GoalAtLocation -- a goal that must hold at a given program location; used by borrow checking
  ;; to implement flow sensitivity
  (GoalAtLocations ::= (GoalAtLocation ...))
  (GoalAtLocation ::= (Location Goal))

  ;; A move set stores a set of places that are moved
  (MoveSet ::= Places)

  ;; Maps from a location to a moveset on entry to that point.
  (MoveSetMap ::= [(Location MoveSet) ...])

  ;; The move-sets before each statement in a block, along with the final move-set
  ;; (the state before the terminator is executed).
  (MoveSetsForStatements ::= ([(MoveSet Statement) ...] MoveSet))

  ;; Identifies the local variables live before entering a given basic block.
  (LiveVariablesMap ::= [(Location LocalIds) ...])
  (LivenessEffects ::= (reads: LocalIds drops: LocalIds))

  ;; Summarizes one possible effect of a terminator:
  ;;
  ;; If control-flow goes backwards from the given basic block,
  ;; then `LivenessEffects` are applied.
  (TerminatorLivenessEffects ::= (BasicBlockId LivenessEffects))

  ;; Liveness constraints encode which lifetimes are live at each point in the source code.
  ;; These are the basis for both the NLL and polonius analyses.
  ;;
  ;; A constraint like `VarId -outlives- L` indicates that the lifetime
  ;; variable `VarId` appears in the type of some local variable `_n`
  ;; which is live at the location `L`.
  (LivenessConstraints ::= [LivenessConstraint ...])
  (LivenessConstraint ::= (VarId -outlives- Location))

  ;; Cfg -- representation of the control-flow-graph. Each node is either a statement
  ;; or a terminator and there are edges. The first node in the list is the entry point.
  (Cfg ::= (LocatedCfgNodes CfgEdges))
  (LocatedCfgNodes ::= [LocatedCfgNode ...])
  (LocatedCfgNode ::= (Location CfgNode))
  (CfgNode ::= Statement Terminator)
  (CfgEdges ::= [CfgEdge ...])
  (CfgEdge ::= (Location Location))

  (DataflowMode ::= ForwardDataflowMode ReverseDataflowMode)
  (ForwardDataflowMode ::= moved-places active-loans)
  (ReverseDataflowMode ::= LivenessMode)
  (LivenessMode ::= use-live drop-live)
  (LocatedCfgValues ::= [LocatedCfgValue ...])
  (LocatedCfgValue ::= (Location CfgValue))
  (CfgValue ::=
            MoveSet ; for moved-places mode
            LoanSet ; for active-loans mode
            LocalIds ; for liveness analyses
            )

  ; A *Loan* occurs when we have a borrow
  (LocatedLoanSets ::= [LocatedLoanSet ...])
  (LocatedLoanSet ::= (Location LoanSet))
  (LoanSet ::= [Loan ...])
  (Loan ::= (Lt MaybeMut Place))

  ;; PlaceAccess -- the ways the code can access a place
  (PlaceAccess ::= read-place write-place storage-dead)

  ;; An `RvalueAction` indicates the kind of things an rvalue can do
  (RvalueActions ::= [RvalueAction ...])
  (RvalueAction ::=
                Operand
                (ref MaybeMut Place)
                )

  ; identifiers of various kinds:
  (LocalIds ::= [LocalId ...])
  (MirId BasicBlockId LocalId ::= variable-not-otherwise-mentioned)
  )

(define-metafunction formality-body
  ;; Returns the `MirBody` from the environment Γ
  locals-and-blocks-of-Γ : Γ -> LocalsAndBlocks

  [(locals-and-blocks-of-Γ (_ _ _ _ LocalsAndBlocks)) LocalsAndBlocks]
  )

(define-metafunction formality-body
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  local-decls-of-Γ : Γ -> LocalDecls

  [(local-decls-of-Γ Γ)
   LocalDecls
   (where/error (LocalDecls _) (locals-and-blocks-of-Γ Γ))]
  )


(define-metafunction formality-body
  ;; Returns the type of the given local variable from the environment Γ
  local-ty : Γ LocalId -> Ty

  [(local-ty Γ LocalId)
   Ty
   (where/error (_ ... (LocalId Ty _) _ ...) (local-decls-of-Γ Γ))
   ]
  )

(define-metafunction formality-body
  ;; Returns the types of the fn arguments from the environment Γ
  argument-types-of-Γ : Γ -> Tys

  [(argument-types-of-Γ Γ)
   Tys
   (where/error (_ _ (Tys -> Ty where Biformulas) _ _) Γ)]
  )

(define-metafunction formality-body
  ;; Returns  the fn return type from the environment Γ
  return-type-of-Γ : Γ -> Tys

  [(return-type-of-Γ Γ)
   Ty
   (where/error (_ _ (Tys -> Ty where Biformulas) _ _) Γ)]
  )

(define-metafunction formality-body
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  basic-block-decls-of-Γ : Γ -> BasicBlockDecls

  [(basic-block-decls-of-Γ Γ)
   BasicBlockDecls
   (where/error (_ BasicBlockDecls) (locals-and-blocks-of-Γ Γ))]
  )

(define-metafunction formality-body
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  entry-basic-block-id-of-Γ : Γ -> BasicBlockId

  [(entry-basic-block-id-of-Γ Γ)
   BasicBlockId_entry
   (where/error [(BasicBlockId_entry _) BasicBlockDecl ...] (basic-block-decls-of-Γ Γ))]
  )

(define-metafunction formality-body
  ;; Returns the `CrateDecls` from the environment Γ
  crate-decls-of-Γ : Γ -> CrateDecls

  [(crate-decls-of-Γ (CrateDecls _ _ _ _)) CrateDecls]
  )
