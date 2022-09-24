#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         )
(provide trait-item-decl-rules
         outlives-clauses
         )

(define-metafunction formality-decl
  ;; Given the name of a trait and some item within that trait, Return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold
  trait-item-decl-rules : CrateDecls CrateId (TraitId KindedVarIds_trait) TraitItem -> (Clauses Invariants)

  [;; For a method declared in the trait Bar with parameter Self, like the following:
   ;;
   ;;     fn foo<T>(x: T) -> T where T: Ord { ... }
   ;;
   ;; We generate the following clauses
   ;;
   ;;     (∀ [(type Self) (type T)]
   ;;         (well-formed-fn foo[Self T]) :-
   ;;            (well-formed (type Self))
   ;;            (well-formed (type T))
   ;;            (is-implemented (Bar Self)))
   ;;            (is-implemented (Ord T)))
   ;;
   ;; And the following invariants local to the crate C:
   ;;
   ;;     (∀ [(type Self) (type T)]
   ;;         (well-formed (type foo[Self T])) => (is-implemented (Ord T)))
   ;;
   ;; And the following global invariants:
   ;;
   ;;     (∀ [(type Self) (type T)]
   ;;         (well-formed (type foo[Self T])) => (&& (well-formed (type Self))
   ;;                                                 (well-formed (type T))))
   (trait-item-decl-rules CrateDecls CrateId (TraitId (KindedVarId_trait ...)) FnDecl)
   ([Clause_wf-fn] [Invariant_well-formed-where-clause ...
                    Invariant_in-scope-where-clause ...
                    Invariant_in-scope-component ...
                    Invariant_well-formed-component ...
                    ])

   (where/error (fn FnId (KindedVarId_fn ...) _ -> _ where (Biformula ...) _) FnDecl)
   (where/error (KindedVarId_all ...) (KindedVarId_trait ... KindedVarId_fn ...))
   (where/error ((ParameterKind_t VarId_t) ...) (KindedVarId_trait ...))
   (where/error ((ParameterKind_f VarId_f) ...) (KindedVarId_fn ...))
   (where/error Ty_fn (rigid-ty (fn-def FnId) (VarId_t ... VarId_f ...)))

   (where/error Clause_wf-fn (∀ (KindedVarId_all ...)
                                 (implies
                                  ((well-formed KindedVarId_all) ...
                                   (is-implemented (TraitId (VarId_t ...)))
                                   Biformula ...)
                                  (well-formed-fn Ty_fn))))

   ; if you have (well-formed (type (foo T))) in env, you get the full where clauses
   ;
   ; with the expanded-implied-bounds flag, this is all the time, but otherwise it's
   ; only in higher-ranked code
   (where/error [Invariant_well-formed-where-clause ...] ((∀ (KindedVarId_all ...)
                                                             (implies
                                                              ((well-formed (type Ty_fn)))
                                                              Biformula))
                                                          ...))

   ; if you have (in-scope (type (foo T))) in env, you get the full where clauses
   ;
   ; with the expanded-implied-bounds flag, this is all the time, but otherwise it's
   ; only in higher-ranked code
   (where/error [Biformula_outlives ...] (outlives-clauses (Biformula ...)))
   (where/error [Invariant_in-scope-where-clause ...] ((∀ (KindedVarId_all ...)
                                                          (implies
                                                           ((in-scope (type Ty_fn)))
                                                           Biformula_outlives))
                                                       ...))

   ; if you have (in-scope (type (foo T))) in env, you also know T is in-scope
   ; and same with (well-formed)
   (where/error [Invariant_in-scope-component ...] ((∀ (KindedVarId_all ...)
                                                       (implies
                                                        ((in-scope (type Ty_fn)))
                                                        (in-scope KindedVarId_all)))
                                                    ...))
   (where/error [Invariant_well-formed-component ...] ((∀ (KindedVarId_all ...)
                                                          (implies
                                                           ((well-formed (type Ty_fn)))
                                                           (well-formed KindedVarId_all)))
                                                       ...))
   ]

  [;; For an associated type declared in a trait, defines
   ;;
   ;; * `well-formed-alias (alias-ty AliasName ...)` clause for when this alias is WF
   ;; * invariants that let us assume things about WF alias types or about
   ;;   types that normalize to WF alias types
   (trait-item-decl-rules CrateDecls CrateId (TraitId (KindedVarId_t ...)) AssociatedTyDecl)
   ([Clause_wf-alias] [Invariant_bound ... Invariant_X ...])

   (where/error ((ParameterKind_t VarId_t) ...) (KindedVarId_t ...))
   (where/error (type AssociatedTyId (KindedVarId_i ...) BoundsClause_i where (Biformula_i ...)) AssociatedTyDecl)
   (where/error ((ParameterKind_i VarId_i) ...) (KindedVarId_i ...))
   (where/error AliasTy (alias-ty (TraitId AssociatedTyId) (VarId_t ... VarId_i ...)))

   ;; the alias type `(alias-ty (TraitId AssociatedTyId) Parameters)` is well-formed if...
   ;; (a) the trait `TraitId` is implemented for its parameters
   ;; (b) the where-clauses from the associated type hold
   (where/error Clause_wf-alias (∀ (KindedVarId_t ... KindedVarId_i ...)
                                   (implies
                                    [(is-implemented (TraitId (VarId_t ...))) ; (a)
                                     Biformula_i ... ; (b)
                                     ]
                                    (well-formed-alias AliasTy))))

   ;; we can conclude that a well-formed alias-ty meets its bounds
   ;;
   ;; e.g. given `type Item: Sized`, we add a rule like
   ;;
   ;; ```
   ;; (is-implemented Sized ((alias-ty (Iterator Item) (T)))) :-
   ;;     (well-formed (alias-ty (Iterator Item) (T)))
   ;; ```
   (where/error [Clause_if-wf ...] (instantiate-bounds-clause BoundsClause_i AliasTy))
   (where/error [Invariant_bound ...] [(∀ (KindedVarId_t ... KindedVarId_i ...)
                                          (implies ((well-formed (type AliasTy))) Clause_if-wf)) ...])

   ;; if we know that an alias type normalizes to another type X, we know
   ;; that X meets the bounds of that alias type too (feature-gated)
   (where/error (VarId_X) (fresh-var-ids CrateDecls (X)))
   (where/error (Clause_X ...) (instantiate-bounds-clause BoundsClause_i VarId_X))
   (where/error [Invariant_X ...] (if-crate-has-feature
                                   CrateDecls
                                   CrateId
                                   expanded-implied-bounds
                                   [; with expanded-implied-bounds, include the invariants
                                    (∀ (KindedVarId_t ... KindedVarId_i ... (type VarId_X))
                                       (implies ((normalizes-to ((normalizes-to AliasTy VarId_X)))) Clause_X)) ...]
                                   [; without expanded-implied-bounds, do not, making us behave like rustc
                                    ]
                                   ))
   ]
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
