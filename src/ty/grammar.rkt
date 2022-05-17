#lang racket
(require redex/reduction-semantics
         racket/set
         "../logic/grammar.rkt"
         )
(provide (all-defined-out))

; Naming convention:
;
; * Rust keywords use the Rust keyword
; * Nonterminals and variables use CamelCase, as do random VariantNames not part of Rust syntax,
;   like TyAdt

(define-extended-language formality-ty formality-logic
  ;; A *scheme* consists of some term and some variables/implications.
  ;;
  ;; FIXME: give a real explanation
  (Schemes ::= (Scheme ...))
  (Scheme ::= (∃ KindedVarIds (implies Goals Term)))

  ;; ParameterKind: kind for a bound variable (type,
  ;; lifetime, etc)
  ;;
  ;; Overridden from formality-logic.
  (ParameterKind ::= type lifetime)

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
             (is-implemented TraitRef)
             ; an impl exists for `TraitRef`; this *by itself* doesn't mean
             ; that `TraitRef` is implemented, as the supertraits may not
             ; have impls.
             (has-impl TraitRef)
             ; the given type or lifetime is well-formed.
             (well-formed (ParameterKind Parameter))
             ; normalize a given alias to another type
             (normalizes-to AliasTy Ty)
             )
  ;; ANCHOR_END:Predicates

  ;; A *deboned* predicate separates out the "rigid part"
  ;; (the skeleton) from the parameters. To determine whether two
  ;; predicates ate equal, the skeletons can just be compared for
  ;; equality, but the parameters have to be equated as types.
  (Predicate/Deboned ::= (Predicate/Skeleton Parameters))
  (Predicate/Skeleton ::=
                      (is-implemented TraitId)
                      (has-impl TraitId)
                      (well-formed ParameterKind)
                      (normalizes-to AliasName)
                      )

  ;; WhereClause -- Rust where clauses. These are a subset of
  ;; the logical Goals, Clauses, and Predicates. They
  ;; can be translated into predicates.
  (WhereClauses ::= (WhereClause ...))
  (WhereClause ::=
               (∀ KindedVarIds WhereClause)
               (is-implemented TraitRef)
               (outlives (Parameter : Lt))
               (normalizes-to AliasTy Ty)
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
  (Ty ::= RigidTy AliasTy PredicateTy VarId)

  ;; RigidTy -- A *rigid* type is one that can only be equal to itself. Most Rust types fit
  ;; this category, e.g., `Vec<i32>` would be represented as `(rigid-ty Vec ((rigid-ty i32 ())))`.
  (RigidTy ::= (rigid-ty RigidName Parameters))
  (RigidName ::=
             AdtId           ; enum/struct/union
             ScalarId        ; Something like i32, u32, etc
             (Ref MaybeMut)  ; `&mut` or `&`, expects a lifetime + type parameter
             (Tuple number)  ; tuple of given arity
             (Fn Abi number) ; fn types
             )

  ;; AliasTy -- an *alias* type is basically a *type lambda*. You can either *normalize* it
  ;; to another type *or* prove that it is equal to another alias type by showing
  ;; that the alias name + arguments are the same.
  (AliasTy ::= (TyAlias AliasName Parameters))
  (AliasName ::=
             AliasId
             (TraitId AssociatedTyId)
             )

  ;; Predicate types correspond to the builtin logical connectives.
  (PredicateTy ::= ∀Ty ∃Ty ImplicationTy EnsuresTy)

  ;; ∀ and implication types: In Rust, these are always paired with `dyn` and `fn` types,
  ;; but in our calculus we separate and generalize them.
  ;;
  ;; Implication types have an interesting twist: if the implication is false, the only
  ;; valid operation on the type is to drop it.
  (∀Ty ::= (∀ KindedVarIds Ty))
  (ImplicationTy ::= (implies WhereClauses Ty))

  ;; ∃ and ensures types: These are used in Rust to model
  (∃Ty ::= (∃ KindedVarIds Ty))
  (EnsuresTy ::= (Ensures Ty WhereClauses))

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

  ;; InequalityOp -- Relations beyond `==`
  ;;
  ;; Overridden from formal-logic.
  (InequalityOp ::=
                SubtypeOp
                OutlivesOp)
  (SubtypeOp ::=
             <=                 ; A "subset" of B (e.g., subtype means A inhabited by subset of values of B)
             >=                 ; A "superset" of B (e.g., supertype)
             )
  (OutlivesOp ::=
              -outlives-         ; `A:B` -- anything that invalidates A must invalidate B
              -outlived-by-      ; `B:A` -- anything that invalidates B must invalidate A
              )

  ;; Scalars -- numbers, booleans
  (ScalarId ::= i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 bool)

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  (AdtId AliasId TraitId AssociatedTyId TyAliasId ::=
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
  (rigid-ty (Tuple 0) ())
  )

(define-metafunction formality-ty
  scalar-ty : ScalarId -> Ty

  ((scalar-ty ScalarId) (rigid-ty ScalarId ()))
  )

(define-metafunction formality-ty
  & : Lt Ty -> Ty

  [(& Lt Ty) (rigid-ty (Ref ()) (Lt Ty))]
  )

(define-metafunction formality-ty
  &mut : Lt Ty -> Ty

  [(&mut Lt Ty) (rigid-ty (Ref (mut)) (Lt Ty))]
  )

(define-metafunction formality-ty
  box : Ty -> Ty

  [(box Ty) (rigid-ty Box (Lt Ty))]
  )

(define-metafunction formality-ty
  vec : Ty -> Ty

  [(vec Ty) (rigid-ty Vec (Lt Ty))]
  )

(define-metafunction formality-ty
  fn : Tys Ty -> Ty

  [(fn (Ty_arg ...) Ty_ret)
   (rigid-ty (Fn "Rust" number) (Ty_arg ... Ty_ret))
   (where/error number ,(length (term (Ty_arg ...))))
   ]
  )
