#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
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
   ;; We generate the following clauses
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed-adt (Foo (T))) :-
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
   (crate-item-decl-rules CrateDecls CrateId (AdtKind AdtId KindedVarIds where (Biformula ...) AdtVariants))
   ([Clause_wf-adt] [Invariant_well-formed-where-clause ...
                     Invariant_in-scope-where-clause ...
                     Invariant_in-scope-component ...
                     Invariant_well-formed-component ...
                     ])

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error Ty_adt (rigid-ty AdtId (VarId ...)))
   (where/error Clause_wf-adt (∀ KindedVarIds
                                 (implies
                                  ((well-formed (ParameterKind VarId)) ...
                                   Biformula ...)
                                  (well-formed-adt Ty_adt))))

   ; if you have (well-formed (type (Foo T))) in env, you get the full where clauses
   ;
   ; with the expanded-implied-bounds flag, this is all the time, but otherwise it's
   ; only in higher-ranked code
   (where/error [Invariant_well-formed-where-clause ...] ((∀ KindedVarIds
                                                             (implies
                                                              ((well-formed (type Ty_adt)))
                                                              Biformula))
                                                          ...))

   ; if you have (in-scope (type (Foo T))) in env, you get the full where clauses
   ;
   ; with the expanded-implied-bounds flag, this is all the time, but otherwise it's
   ; only in higher-ranked code
   (where/error [Biformula_outlives ...] (outlives-clauses (Biformula ...)))
   (where/error [Invariant_in-scope-where-clause ...] ((∀ KindedVarIds
                                                          (implies
                                                           ((in-scope (type Ty_adt)))
                                                           Biformula_outlives))
                                                       ...))

   ; if you have (in-scope (type (Foo T))) in env, you also know T is in-scope
   ; and same with (well-formed)
   (where/error [Invariant_in-scope-component ...] ((∀ KindedVarIds
                                                       (implies
                                                        ((in-scope (type Ty_adt)))
                                                        (in-scope (ParameterKind VarId))))
                                                    ...))
   (where/error [Invariant_well-formed-component ...] ((∀ KindedVarIds
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
   ;;            (well-formed (type Self))
   ;;            (well-formed (lifetime 'a))
   ;;            (well-formed (type T))
   ;;            (is-implemented (Ord T))
   ;;     )
   ;;
   ;; We also generate the following invariants in the defining crate:
   ;;
   ;;     (∀ ((type Self) (lifetime 'a) (type T))
   ;;         (is-implemented (Foo (Self 'a T))) => (is-implemented (Ord T))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (type Self))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (lifetime 'a))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (type T)))
   (crate-item-decl-rules CrateDecls CrateId  (trait TraitId KindedVarIds where (Biformula ...) (TraitItem ...)))
   ((flatten ([Clause_is-impl Clause_is-wf] Clauses_item ...))
    (flatten ((Invariant_well-formed ... Invariant_where-clause ...) Invariants_item ...)))

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error TraitRef_me (TraitId (VarId ...)))

   ; Clause for `(is-implemented (TraitId Parameters))` -- trait is implemented if
   ; (a) there's an impl for it,
   ; (b) it's well-formed (meets the where-clauses from the TraitId)
   (where/error Clause_is-impl (∀ KindedVarIds
                                  (implies
                                   ((has-impl TraitRef_me) ; (a)
                                    (well-formed-trait-ref TraitRef_me)
                                    )
                                   (is-implemented TraitRef_me))))

   ; Clause for `(well-formed-trait-ref (TraitId Parameters))` -- trait is WF if
   ; (a) all where-clauses hold
   ; (b) input types are well-formed
   (where/error Clause_is-wf (∀ KindedVarIds
                                (implies
                                 [Biformula ... ; (a)
                                  (well-formed (ParameterKind VarId)) ... ; (b)
                                  ]
                                 (well-formed-trait-ref TraitRef_me))))

   ; Invariants: if you know that `T: Foo`, you also know `T: Bar`
   ; where `Bar` is a supertrait (normal) / where-clause (expanded-implied-bounds)
   (where/error [Biformula_implied ...] (if-crate-has-feature
                                         CrateDecls
                                         CrateId
                                         expanded-implied-bounds
                                         [; with the `expanded-implied-bounds` feature, you get all the where clauses
                                          Biformula ...]
                                         [; without the `expanded-implied-bounds` feature, you only get the super traits
                                          super-where-clauses KindedVarIds (Biformula ...)]
                                         ))
   (where/error [Invariant_where-clause ...] [(∀ KindedVarIds
                                                 (implies
                                                  ((is-implemented TraitRef_me))
                                                  Biformula_implied)) ...])
   (where/error [Invariant_well-formed ...] (if-crate-has-feature
                                             CrateDecls
                                             CrateId
                                             expanded-implied-bounds
                                             [(∀ KindedVarIds
                                                 (implies
                                                  ((is-implemented TraitRef_me))
                                                  (well-formed (ParameterKind VarId)))) ...]
                                             []))

   ; get program-clauses and invariants from the trait items
   (where/error [(Clauses_item Invariants_item) ...] [(trait-item-decl-rules CrateDecls CrateId (TraitId KindedVarIds) TraitItem) ...])
   ]

  [;; For an trait impl declared in the crate C, like the followin
   ;;
   ;;     impl<'a, T> Foo<'a, T> for i32 where T: Ord { }
   ;;
   ;; We consider `has-impl` to hold if (a) all inputs are well formed and (b) where
   ;; clauses are satisfied:
   ;;
   ;;     ∀ ((lifetime 'a) (type T))
   ;;         (has-impl (Foo (i32 'a u32))) :-
   ;;             (is-implemented (Ord T))
   ;;             (well-formed (type i32))
   ;;             (well-formed (lifetime 'a))
   (crate-item-decl-rules CrateDecls CrateId (impl KindedVarIds_impl TraitRef where [Biformula_impl ...] [ImplItem ...]))
   ((flatten ([Clause]
              Clauses_item ...))
    (flatten (Invariants_item ...))
    )

   (where/error (TraitId (Parameter_trait ...)) TraitRef)
   (where/error (trait TraitId KindedVarIds_trait where _ _) (trait-with-id CrateDecls TraitId))
   (where/error ((ParameterKind_trait _) ...) KindedVarIds_trait)
   (where/error Clause (∀ KindedVarIds_impl
                          (implies
                           (Biformula_impl ...
                            (well-formed (ParameterKind_trait Parameter_trait)) ...
                            )
                           (has-impl TraitRef))))
   (where/error [(Clauses_item Invariants_item) ...]
                [(impl-item-decl-rules CrateDecls
                                       CrateId
                                       (impl KindedVarIds_impl TraitRef where [Biformula_impl ...])
                                       ImplItem) ...])
   ]

  [;; For a function declared in the crate C, like the following:
   ;;
   ;;     fn foo<'a, T>(&'a T) -> &'a T where T: Ord { ... }
   ;;
   ;; We generate the following clauses
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed-fn (foo (T))) :-
   ;;            (well-formed (type T))
   ;;            (is-implemented (Ord T)))
   ;;
   ;; And the following invariants local to the crate C:
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (foo (T)))) => (is-implemented (Ord T)))
   ;;
   ;; And the following global invariants:
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (foo (T)))) => (well-formed (type T)))
   (crate-item-decl-rules CrateDecls CrateId (fn FnId KindedVarIds _ -> _ where (Biformula ...) _))
   ([Clause_wf-fn] [Invariant_well-formed-where-clause ...
                    Invariant_in-scope-where-clause ...
                    Invariant_in-scope-component ...
                    Invariant_well-formed-component ...
                    ])

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error Ty_fn (rigid-ty (fn-def FnId) (VarId ...)))
   (where/error Clause_wf-fn (∀ KindedVarIds
                                 (implies
                                  ((well-formed (ParameterKind VarId)) ...
                                   Biformula ...)
                                  (well-formed-fn Ty_fn))))

   ; if you have (well-formed (type (foo T))) in env, you get the full where clauses
   ;
   ; with the expanded-implied-bounds flag, this is all the time, but otherwise it's
   ; only in higher-ranked code
   (where/error [Invariant_well-formed-where-clause ...] ((∀ KindedVarIds
                                                             (implies
                                                              ((well-formed (type Ty_fn)))
                                                              Biformula))
                                                          ...))

   ; if you have (in-scope (type (foo T))) in env, you get the full where clauses
   ;
   ; with the expanded-implied-bounds flag, this is all the time, but otherwise it's
   ; only in higher-ranked code
   (where/error [Biformula_outlives ...] (outlives-clauses (Biformula ...)))
   (where/error [Invariant_in-scope-where-clause ...] ((∀ KindedVarIds
                                                          (implies
                                                           ((in-scope (type Ty_fn)))
                                                           Biformula_outlives))
                                                       ...))

   ; if you have (in-scope (type (foo T))) in env, you also know T is in-scope
   ; and same with (well-formed)
   (where/error [Invariant_in-scope-component ...] ((∀ KindedVarIds
                                                       (implies
                                                        ((in-scope (type Ty_fn)))
                                                        (in-scope (ParameterKind VarId))))
                                                    ...))
   (where/error [Invariant_well-formed-component ...] ((∀ KindedVarIds
                                                          (implies
                                                           ((well-formed (type Ty_fn)))
                                                           (well-formed (ParameterKind VarId))))
                                                       ...))
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
  super-where-clauses : KindedVarIds Biformulas -> Biformulas

  ((super-where-clauses KindedVarIds (Biformula ...))
   (flatten ((filter-super-where-clauses KindedVarIds Biformula) ...))
   )

  )

(define-metafunction formality-decl
  filter-super-where-clauses : KindedVarIds Biformula -> Biformulas

  (; Keep `Self: Trait` bounds
   (filter-super-where-clauses KindedVarIds (is-implemented (TraitId [VarId_Self Parameter ...])))
   ((is-implemented (TraitId [VarId_Self Parameter ...])))
   (where ((type VarId_Self) _ ...) KindedVarIds)
   )

  (; Keep `Self: 'a` bounds
   (filter-super-where-clauses KindedVarIds (VarId_Self -outlives- Parameter))
   ((VarId_Self -outlives- Parameter))
   (where ((type VarId_Self) _ ...) KindedVarIds)
   )

  (; Discard others
   (filter-super-where-clauses KindedVarIds Biformula)
   ()
   )

  )

(define-metafunction formality-decl
  outlives-clauses : Biformulas -> Biformulas

  ((outlives-clauses (Biformula ...))
   (flatten ((filter-outlives-where-clause Biformula) ...))
   )

  )

(define-metafunction formality-decl
  filter-outlives-where-clause : Biformula -> Biformulas

  (; Keep `P1 : P2` bounds
   (filter-outlives-where-clause (Parameter_1 -outlives- Parameter_2))
   ((Parameter_1 -outlives- Parameter_2))
   )

  (; Discard others
   (filter-outlives-where-clause Biformula)
   ()
   )

  )