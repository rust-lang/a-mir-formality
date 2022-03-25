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

  ;; Ty -- Rust types
  ;;
  ;; A type `Ty` can be a `VarId`, in which case it is
  ;; either an existential (inference) variable or a bound
  ;; variable. It can also be a `TyNotVar`, in which case
  ;; it is a known type.
  ;;
  ;; Generic types are represented using `TyRigid` inside their
  ;; scope. So for example given `fn foo<T>() { ... }`,
  ;; inside the body of `foo`, we would represent `T` with
  ;; `(TyRigid T ())`.
  (Tys ::= (Ty ...))
  (Ty ::=
      (TyRigid RigidName Parameters) ; A *rigid* type is onee that can only be equal to itself.
      VarId                          ; Bound, inference (existential), or placeholder (universal) variable
      )
  (RigidName ::=
             AdtId           ; enum/struct/union
             TraitId         ; trait
             AssociatedTy    ; Associated type
             ScalarId        ; Something like i32, u32, etc
             (Ref MaybeMut)  ; `&mut` or `&`, expects a lifetime + type parameter
             (Tuple number)  ; tuple of given arity
             )

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
  (AssociatedTyApplication ::= (AssociatedTy Parameters))

  ;; MaybeMut: either mut or not
  (MaybeMut ::= () (mut))

  ;; Scalars -- numbers, booleans
  (ScalarId ::= i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 bool)

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  (AdtId TraitId AssociatedTyId ::=
         variable-not-otherwise-mentioned)
  )

(define-term
  TyUnit
  (TyRigid (Tuple 0) ())
  )

(define-metafunction formality-ty
  scalar-ty : ScalarId -> Ty

  ((scalar-ty ScalarId) (TyRigid ScalarId ()))
  )
