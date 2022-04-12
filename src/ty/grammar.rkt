#lang racket
(require redex/reduction-semantics
         racket/set
         "../logic/grammar.rkt"
         )
(provide formality-ty
         TyUnit
         scalar-ty
         )

; Naming convention:
;
; * Rust keywords use the Rust keyword
; * Nonterminals and variables use CamelCase, as do random VariantNames not part of Rust syntax,
;   like TyAdt

(define-extended-language formality-ty formality-logic
  ;; ParameterKind: kind for a bound variable (type,
  ;; lifetime, etc)
  ;;
  ;; Overridden from formality-logic.
  (ParameterKind ::= TyKind LtKind)

  ;; Parameter: value for a generic parameter
  ;;
  ;; Overridden from formality-logic.
  (Parameter ::= Ty Lt)

  ;; ANCHOR:Predicates
  ;; `Predicate` -- the atomic items that we can prove
  ;;
  ;; Overridden from formality-logic.
  (Predicate ::=
             ; `TraitRef` is (fully) implemented.
             (Implemented TraitRef)
             ; an impl exists for `TraitRef`; this *by itself* doesn't mean
             ; that `TraitRef` is implemented, as the supertraits may not
             ; have impls.
             (HasImpl TraitRef)
             ; the given type or lifetime is well-formed.
             (WellFormed (ParameterKind Parameter))
             )
  ;; ANCHOR_END:Predicates

  ;; A *flayed* predicate separates out the "rigid part"
  ;; (the skeleton) from the parameters. To determine whether two
  ;; predicates ate equal, the skeletons can just be compared for
  ;; equality, but the parameters have to be equated as types.
  (Predicate/Flayed ::= (Predicate/Skeleton Parameters))
  (Predicate/Skeleton ::=
                      (Implemented TraitId)
                      (HasImpl TraitId)
                      (WellFormed ParameterKind)
                      )

  ;; WhereClause -- Rust where clauses. These are a subset of
  ;; the logical Goals, Clauses, and Predicates. They
  ;; can be translated into predicates.
  (WhereClauses ::= (WhereClause ...))
  (WhereClause ::=
               (ForAll KindedVarIds WhereClause)
               (Implemented TraitRef)
               #;(Outlives (Parameter : Lt))
               #;(ProjectionEq TraitRef :: (AssociatedTyId Substitution) = Ty)
               )

  ;; Ty -- Rust types
  ;;
  ;; Most Rust types are some variation of `RigidTy`,
  ;; but there are a number of different varieties.
  ;; See the documentation on those specific nonterminals
  ;; for more information.
  ;;
  ;; A type `Ty` can be a `VarId`, in which case it is
  ;; either a bound, existential (inference), or universal
  ;; (placeholder) variable.
  (Tys ::= (Ty ...))
  (Ty ::= RigidTy ForAllTy ImplicationTy VarId)

  ;; RigidTy -- A *rigid* type is onee that can only be equal to itself. Most Rust types fit
  ;; this category, e.g., `Vec<i32>` would be represented as `(TyRigid Vec ((TyRigid i32 ())))`.
  (RigidTy ::= (TyRigid RigidName Parameters))
  (RigidName ::=
             AdtId           ; enum/struct/union
             ScalarId        ; Something like i32, u32, etc
             (Ref MaybeMut)  ; `&mut` or `&`, expects a lifetime + type parameter
             (Tuple number)  ; tuple of given arity
             (Fn Abi number) ; fn types
             )

  ;; ForAll/Implication Types: In Rust, these are always paired with `dyn` and `fn` types,
  ;; but in our calculus we separate and generalize them.
  ;;
  ;; Implication types have an interesting twist: if the implication is false, the only
  ;; valid operation on the type is to drop it.
  (ForAllTy ::= (ForAll KindedVarIds Ty))
  (ImplicationTy ::= (Implies WhereClauses Ty))

  ;; Treat ABIs as opaque strings (for now, at least)
  (Abi ::= string)

  ;; Lt -- Rust lifetimes
  ;;
  ;; Very similar to types `Ty` in terms of how they are represented
  ;; and the meaning of `VarId`.
  (Lt ::=
      static                      ; 'static
      VarId                       ; Bound, inference (existential), or placeholder (universal) variable
      )

  ;; TraitRef = reference to a trait
  (TraitRef ::= (TraitId Parameters))

  ;; TraitRef = reference to a trait
  (AssociatedTy ::= (TraitId AssociatedTyId))

  ;; MaybeMut: either mut or not
  (MaybeMut ::= () (mut))

  ;; Pairs of variables (X0, X1), used as a kind of map
  (VarIdPairs ::= (VarIdPair ...))
  (VarIdPair ::= (VarId VarId))

  ;; VarInequality -- Stores the bounds for a variable (type, lifetime)
  ;; whose value is not known precisely.
  ;;
  ;; Overridden from formal-logic.
  (VarInequality ::= (VarId InequalityOp Parameters))

  ;; Scalars -- numbers, booleans
  (ScalarId ::= i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 bool)

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  (AdtId TraitId AssociatedTyId TyAliasId ::=
         variable-not-otherwise-mentioned)

  ;; Generic parameters
  (Generics ::= (GenericParameters WhereClauses))
  (GenericParameters ::= (GenericParameter ...))
  (GenericParameter ::= (VarId KindAndVariance))
  (KindAndVariance ::= (ParameterKind Variance))
  (Variance := - + =)
  )

(define-term
  TyUnit
  (TyRigid (Tuple 0) ())
  )

(define-metafunction formality-ty
  scalar-ty : ScalarId -> Ty

  ((scalar-ty ScalarId) (TyRigid ScalarId ()))
  )
