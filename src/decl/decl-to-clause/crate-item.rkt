#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../where-clauses.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         "trait-item.rkt"
         "impl-item.rkt"
         )
(provide crate-item-decl-rules
         )

(define-metafunction formality-decl
  ;; Given:
  ;;
  ;; * a set of crate-declarations
  ;; * the id I of a crate
  ;; * some item in the the crate I
  ;;
  ;; Return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold
  crate-item-decl-rules : CrateDecls CrateId CrateItemDecl -> (Clauses Invariants)

  [;; For an ADT declaration declared in the crate C, like the following:
   ;;
   ;;     struct Foo<T> where T: Ord { ... }
   ;;
   ;; We generate the following clause
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (Foo (T)))) :-
   ;;            (well-formed (type T))
   ;;            (is-implemented (Ord T)))
   ;;
   ;; And the following invariants local to the crate C:
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (Foo (T)))) => (is-implemented (Ord T)))
   ;;
   ;; And the following global invariants:
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (Foo (T)))) => (well-formed (type T)))
   (crate-item-decl-rules CrateDecls CrateId (AdtKind AdtId KindedVarIds where (WhereClause ...) AdtVariants))
   ((Clause) (Invariant_well-formed ... Invariant_where-clause ...))

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error Ty_adt (rigid-ty AdtId (VarId ...)))
   (where/error Clause (∀ KindedVarIds
                          (implies
                           ((well-formed (ParameterKind VarId)) ...
                            (where-clause->goal CrateDecls WhereClause) ...)
                           (well-formed (type Ty_adt)))))
   (where/error (WhereClause_implied ...) (if-crate-has-feature
                                           CrateDecls
                                           CrateId
                                           expanded-implied-bounds
                                           (; with the `expanded-implied-bounds` feature, you get all the where clauses
                                            WhereClause ...)
                                           (; without the `expanded-implied-bounds` feature, you only get the super traits
                                            outlives-clauses (WhereClause ...))
                                           ))
   (where/error (Invariant_where-clause ...) ((∀ KindedVarIds
                                                 (implies
                                                  ((well-formed (type Ty_adt)))
                                                  (where-clause->hypothesis CrateDecls WhereClause_implied)))
                                              ...))
   (where/error (Invariant_well-formed ...) ((∀ KindedVarIds
                                                (implies
                                                 ((well-formed (type Ty_adt)))
                                                 (well-formed (ParameterKind VarId))))
                                             ...))
   ]

  [;; For a trait declaration declared in the crate C, like the following:
   ;;
   ;;     trait Foo<'a, T> where T: Ord { ... }
   ;;
   ;; We generate the following clause that proves `Foo` is implemented
   ;; for some types `(Self 'a T)`. Note that, for `Foo` to be considered
   ;; implemented, all of its input types must be well-formed, it must have
   ;; an impl, and the where-clauses declared on the trait must be met:
   ;;
   ;;     (∀ ((type Self) (lifetime 'a) (type T))
   ;;         (is-implemented (Foo (Self 'a T))) :-
   ;;            (has-impl (Foo (Self 'a T))),
   ;;            (well-formed (type Self)), XXX
   ;;            (well-formed (lifetime 'a)), XXX
   ;;            (well-formed (type T)), XXX
   ;;            (is-implemented (Ord T)))
   ;;
   ;; We also generate the following invariants in the defining crate:
   ;;
   ;;     (∀ ((type Self) (lifetime 'a) (type T))
   ;;         (is-implemented (Foo (Self 'a T))) => (is-implemented (Ord T))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (type Self))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (lifetime 'a))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (type T)))
   (crate-item-decl-rules CrateDecls CrateId  (trait TraitId KindedVarIds where (WhereClause ...) (TraitItem ...)))
   ((flatten ((Clause) Clauses_item ...))
    (flatten ((Invariant_well-formed ... Invariant_where-clause ...) Invariants_item ...)))

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error TraitRef_me (TraitId (VarId ...)))

   ; Clause for `(is-implemented Trait ...)` -- trait is implemented if
   ; (a) there's an impl for it,
   ; (b) input types are well-formed XXX
   ; (c) all where-clauses hold
   (where/error Clause (∀ KindedVarIds
                          (implies
                           ((has-impl TraitRef_me) ; (a)
                            ; XXX (well-formed (ParameterKind VarId)) ... ; (b)
                            (where-clause->goal CrateDecls WhereClause) ... ; (c)
                            )
                           (is-implemented TraitRef_me))))


   ; Invariants: if you know that `T: Foo`, you also know `T: Bar`
   ; where `Bar` is a supertrait (normal) / where-clause (expanded-implied-bounds)
   (where/error (WhereClause_implied ...) (if-crate-has-feature
                                           CrateDecls
                                           CrateId
                                           expanded-implied-bounds
                                           (; with the `expanded-implied-bounds` feature, you get all the where clauses
                                            WhereClause ...)
                                           (; without the `expanded-implied-bounds` feature, you only get the super traits
                                            super-where-clauses KindedVarIds (WhereClause ...))
                                           ))
   (where/error (Invariant_where-clause ...)  ((∀ KindedVarIds
                                                  (implies
                                                   ((is-implemented TraitRef_me))
                                                   (where-clause->hypothesis CrateDecls WhereClause_implied))) ...))
   (where/error (Invariant_well-formed ...) ((∀ KindedVarIds
                                                (implies
                                                 ((is-implemented TraitRef_me))
                                                 (well-formed (ParameterKind VarId)))) ...))

   ; get program-clauses and invariants from the trait items
   (where/error ((Clauses_item Invariants_item) ...) ((trait-item-decl-rules CrateDecls CrateId (TraitId KindedVarIds) TraitItem) ...))
   ]

  [;; For an trait impl declared in the crate C, like the followin
   ;;
   ;;     impl<'a, T> Foo<'a, T> for i32 where T: Ord { }
   ;;
   ;; We consider `has-impl` to hold if (a) all inputs are well formed and (b) where
   ;; clauses are satisfied:
   ;;
   ;;     (∀ ((lifetime 'a) (type T))
   ;;         (has-impl (Foo (i32 'a u32))) :-
   ;;             (well-formed (type i32)) XXX
   ;;             (well-formed (lifetime 'a)) XXX
   ;;             (is-implemented (Ord T)))
   (crate-item-decl-rules CrateDecls CrateId (impl KindedVarIds_impl TraitRef where WhereClauses_impl [ImplItem ...]))
   ((flatten ([Clause]
              Clauses_item ...))
    (flatten (Invariants_item ...))
    )

   (where/error (TraitId (Parameter_trait ...)) TraitRef)
   (where/error (trait TraitId KindedVarIds_trait where _ _) (trait-with-id CrateDecls TraitId))
   (where/error ((ParameterKind_trait _) ...) KindedVarIds_trait)
   (where/error (Goal_wc ...) (where-clauses->goals CrateDecls WhereClauses_impl))
   (where/error Clause (∀ KindedVarIds_impl
                          (implies
                           (; XXX (well-formed (ParameterKind_trait Parameter_trait)) ...
                            Goal_wc ...
                            )
                           (has-impl TraitRef))))
   (where/error [(Clauses_item Invariants_item) ...]
                [(impl-item-decl-rules CrateDecls
                                       CrateId
                                       (impl KindedVarIds_impl TraitRef where WhereClauses_impl)
                                       ImplItem) ...])
   ]

  [;; For a function declared in the crate C, like the following
   ;;
   ;;     fn foo<'a, T>(&'a T) -> &'a T { ... }
   (crate-item-decl-rules CrateDecls CrateId (fn _ KindedVarIds_fn Tys_arg -> Ty_ret where WhereClauses_fn FnBody))
   (() ())
   ]

  [;; For an named constant in the crate C, like the following
   ;;
   ;;    const NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we don't create any clauses.
   (crate-item-decl-rules CrateDecls CrateId ConstDecl)
   (() ())
   ]

  [;; For a named static in the crate C, like the following
   ;;
   ;;    static NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we don't create any clauses.
   (crate-item-decl-rules CrateDecls CrateId StaticDecl)
   (() ())
   ]

  [;; Feature gates don't introduce any rules
   (crate-item-decl-rules CrateDecls CrateId FeatureDecl)
   (() ())
   ]
  )

(define-metafunction formality-decl
  super-where-clauses : KindedVarIds WhereClauses -> WhereClauses

  ((super-where-clauses KindedVarIds (WhereClause ...))
   (flatten ((filter-super-where-clauses KindedVarIds WhereClause) ...))
   )

  )

(define-metafunction formality-decl
  filter-super-where-clauses : KindedVarIds WhereClause -> WhereClauses

  (; Keep `Self: Trait` bounds
   (filter-super-where-clauses KindedVarIds (VarId_Self : TraitId Parameters))
   ((VarId_Self : TraitId Parameters))
   (where ((type VarId_Self) _ ...) KindedVarIds)
   )

  (; Keep `Self: 'a` bounds
   (filter-super-where-clauses KindedVarIds (VarId_Self : Parameter))
   ((VarId_Self : Parameter))
   (where ((type VarId_Self) _ ...) KindedVarIds)
   )

  (; Discard others
   (filter-super-where-clauses KindedVarIds WhereClause)
   ()
   )

  )

(define-metafunction formality-decl
  outlives-clauses : WhereClauses -> WhereClauses

  ((outlives-clauses (WhereClause ...))
   (flatten ((filter-outlives-where-clause WhereClause) ...))
   )

  )

(define-metafunction formality-decl
  filter-outlives-where-clause : WhereClause -> WhereClauses

  (; Keep `Self: 'a` bounds
   (filter-outlives-where-clause (Parameter_1 : Parameter_2))
   ((Parameter_1 : Parameter_2))
   )

  (; Discard others
   (filter-outlives-where-clause WhereClause)
   ()
   )

  )