#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         )
(provide trait-item-decl-rules
         )

(define-metafunction formality-decl
  ;; Given the name of a trait and some item within that trait, Return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold
  trait-item-decl-rules : CrateDecls CrateId (TraitId KindedVarIds_trait) TraitItem -> (Clauses Invariants)

  [;; For a method declared in a trait: currently no logical rules are created.
   (trait-item-decl-rules CrateDecls CrateId (TraitId KindedVarIds_trait) FnDecl)
   ([] [])
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
