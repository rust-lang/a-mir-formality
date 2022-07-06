#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../where-clauses.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         "../../logic/grammar.rkt"
         "trait-item.rkt"
         "impl-item.rkt"
         "wf-where-clause.rkt"
         )
(provide crate-item-ok-goal
         )

(define-metafunction formality-decl
  ;; Given a crate item, return a Goal that, if proven, means that the
  ;; declaration is "ok" (well-formed).
  crate-item-ok-goal : CrateDecls CrateItemDecl -> Goal

  [;; For an ADT declaration declared in the crate C, like the following:
   ;;
   ;;     struct Foo<T> where T: Ord { ... f: Vec<T> ... }
   ;;
   ;; We generate the following goal, which specifies that -- assuming the generics
   ;; are well formed and the where-clauses hold -- the field types are well-formed:
   ;;
   ;;     (∀ ((type T))
   ;;         (implies ((well-formed (type T))
   ;;                   (is-implemented (Ord T)))
   ;;           (well-formed (type Vec<T>)) ...))
   (crate-item-ok-goal CrateDecls (AdtKind AdtId KindedVarIds where (WhereClause ...) AdtVariants))
   Goal_wf

   (where/error (KindedVarId ...) KindedVarIds)
   (where/error ((VariantId ((FieldId Ty) ...)) ...) AdtVariants)
   (where/error Goal_wf (∀ KindedVarIds
                           (implies
                            ((well-formed KindedVarId) ... (where-clause->hypothesis CrateDecls WhereClause) ...)
                            (&& ((well-formed (type Ty))
                                 ... ...
                                 (well-formed-where-clause-goal CrateDecls WhereClause)
                                 ...))
                            )))
   ]

  [;; For a fn declaration declared in the crate C, like the following:
   ;;
   ;;     fn foo<'a, T>(t: &'a T) -> u32 where T: Ord { ... }
   ;;
   ;; We generate the following goal, which specifies that -- assuming the generics
   ;; are well formed and the where-clauses hold -- the field types are well-formed:
   ;;
   ;;     (∀ ((lifetime A) (type T))
   ;;         (implies ((well-formed (lifetime A))
   ;;                   (well-formed (type T))
   ;;                   (well-formed (type (rigid-ty (ref ()) (A (rigid-ty T ()))))))
   ;;           (well-formed-where-clause-goal (T : Trait_Ord ()))))
   ;;
   ;; FIXME: Actually implement that, along with for the other items
   (crate-item-ok-goal _ (fn FnId KindedVarIds Tys_arg -> Ty_ret where (WhereClause ...) FnBody))
   (&& ((well-formed-where-clause-goal WhereClause) ...))
   ]

  [;; For a trait declaration declared in the crate C, like the following:
   ;;
   ;;     trait Foo<'a, T> where T: Ord { ... }
   ;;
   ;; we require that (a) trait where-clauses are WF and (b) all the trait-item WF goals are met.
   (crate-item-ok-goal CrateDecls (trait TraitId KindedVarIds where (WhereClause ...) (TraitItem ...)))
   (∀ KindedVarIds
      (implies ((well-formed KindedVarId) ... (where-clause->hypothesis CrateDecls WhereClause) ...)
               (&& (Goal_trait-item ...
                    (well-formed-where-clause-goal CrateDecls WhereClause) ...))))

   (where/error (Goal_trait-item ...) ((trait-item-ok-goal CrateDecls (TraitId KindedVarIds (WhereClause ...)) TraitItem) ...))
   (where/error (KindedVarId ...) KindedVarIds)
   ]

  [;; For a trait impl declared in the crate C, like the following:
   ;;
   ;;     impl<'a, T> Foo<'a, T> for u32 { }
   ;;
   ;; we require that the trait is implemented, given that all generics are WF,
   ;; all inputs are WF, and where-clauses are satisfied.
   (crate-item-ok-goal CrateDecls (impl KindedVarIds_impl (TraitId (Parameter_trait ...)) where WhereClauses_impl (ImplItem ...)))
   (∀ KindedVarIds_impl
      (implies
       (; assuming all generic parameters are WF...
        (well-formed KindedVarId_impl) ...
        ; ...all inputs are WF...
        (well-formed (ParameterKind_trait Parameter_trait)) ...
        ; ...where-clauses are satisfied...
        (where-clause->hypothesis CrateDecls WhereClause_impl) ...)
       (&& ((; ... then the trait must be implemented
             is-implemented (TraitId (Parameter_trait ...)))
            (well-formed-where-clause-goal CrateDecls WhereClause_impl) ...
            Goal_item ...
            ))))

   (where/error (trait TraitId ((ParameterKind_trait _) ...) where _ _) (trait-with-id CrateDecls TraitId))
   (where/error (KindedVarId_impl ...) KindedVarIds_impl)
   (where/error (WhereClause_impl ...) WhereClauses_impl)
   (where/error (Goal_item ...) ((impl-item-ok-goal CrateDecls
                                                    (TraitId (Parameter_trait ...))
                                                    ImplItem) ...))
   ]

  [;; For a constant declared in the crate C, like the following:
   ;;
   ;;     const NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we require that the type is well formed assuming the where clauses are satisfied.
   (crate-item-ok-goal CrateDecls (const ConstId KindedVarIds where WhereClauses : Ty = FnBody))
   (∀ KindedVarIds
      (implies
       (; assuming all generic parameters are WF...
        (well-formed KindedVarId) ...
        ; ...where-clauses are satisfied...
        (where-clause->hypothesis CrateDecls WhereClause) ...)
       (&& ((; ... then the trait must be implemented
             well-formed (type Ty))
            (well-formed-where-clause-goal CrateDecls WhereClause) ...))))

   (where/error (KindedVarId ...) KindedVarIds)
   (where/error (WhereClause ...) WhereClauses)
   ]

  [;; For a static declared in the crate C, like the following:
   ;;
   ;;     static NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we require that the type is well formed assuming the where clauses are satisfied
   ;; and that the type is `Send`.
   (crate-item-ok-goal CrateDecls (static StaticId KindedVarIds where WhereClauses : Ty = FnBody))
   (∀ KindedVarIds
      (implies
       (; assuming all generic parameters are WF...
        (well-formed KindedVarId) ...
        ; ...where-clauses are satisfied...
        (where-clause->hypothesis CrateDecls WhereClause) ...)
       (&& ((; ... then the trait must be implemented
             well-formed (type Ty))
            (; ... and the type must be Sync
             is-implemented (core:Sync (Ty)))
            (; ... and the where-clauses must be WF
             well-formed-where-clause-goal CrateDecls WhereClause)
            ...))))

   (where/error (KindedVarId ...) KindedVarIds)
   (where/error (WhereClause ...) WhereClauses)
   ]

  [;; Features are always ok.
   (crate-item-ok-goal CrateDecls FeatureDecl)
   true-goal
   ]
  )