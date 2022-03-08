#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../ty/grammar.rkt")
(provide crate-item-ok-goal)

(define-metafunction formality-decl
  ;; Given a crate item, return a Goal that, if proven, means that the
  ;; declaration is "ok" (well-formed).
  crate-item-ok-goal : CrateDecls CrateItemDecl -> Goal

  [;; For an ADT declaration declared in the crate C, like the following:
   ;;
   ;;     struct Foo<T> where T: Ord { ... f: T ... }
   ;;
   ;; We generate the following goal, which specifies that -- assuming the generics
   ;; are well formed and the where-clauses hold -- the field types are well-formed:
   ;;
   ;;     (ForAll ((TyKind T))
   ;;         (Implies ((WellFormed (TyKind T))
   ;;                   (Implemented (Ord T)))
   ;;           (WellFormed (TyKind T)) ...))
   (crate-item-ok-goal _ (AdtId (AdtKind KindedVarIds (WhereClause ...) AdtVariants)))
   Goal_wf

   (where/error (KindedVarId ...) KindedVarIds)
   (where/error ((VariantId (FieldId Ty) ...) ...) AdtVariants)
   (where/error Goal_wf (ForAll KindedVarIds
                                (Implies
                                 ((WellFormed KindedVarId) ... WhereClause ...)
                                 (All ((WellFormed (TyKind Ty)) ... ...)))))
   ]

  [;; For a trait declaration declared in the crate C, like the following:
   ;;
   ;;     trait Foo<'a, T> where T: Ord { ... }
   ;;
   ;; we require that all the trait-item WF goals are met.
   (crate-item-ok-goal _ (TraitId (trait KindedVarIds (WhereClause ...) (TraitItem ...))))
   (ForAll KindedVarIds
           (Implies ((WellFormed KindedVarId) ... WhereClause ...)
                    (All (Goal_trait-item ...))))

   (where/error (Goal_trait-item ...) ((trait-item-ok-goal TraitItem) ...))
   (where/error (KindedVarId ...) KindedVarIds)
   ]

  [;; For a trait impl declared in the crate C, like the following:
   ;;
   ;;     impl<'a, T> Foo<'a, T> for u32 { }
   ;;
   ;; we require that the trait is implemented.
   (crate-item-ok-goal _ (impl KindedVarIds_impl TraitRef WhereClauses_impl ImplItems))
   (ForAll KindedVarIds_impl
           (Implies ((WellFormed KindedVarId_impl) ... WhereClause_impl ...)
                    (All ((Implemented TraitRef)))))

   (where/error (KindedVarId_impl ...) KindedVarIds_impl)
   (where/error (WhereClause_impl ...) WhereClauses_impl)
   ]
  )

(define-metafunction formality-decl
  trait-item-ok-goal : TraitItemDecl -> Goal
  )