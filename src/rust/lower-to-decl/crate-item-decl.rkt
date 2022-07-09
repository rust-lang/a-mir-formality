#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         "where-clause.rkt"
         "field-decl.rkt"
         "trait-item.rkt"
         "impl-item.rkt"
         "fn-decl.rkt"
         "in-scope.rkt"
         )
(provide lower-to-decl/CrateItemDecl
         )

(define-metafunction formality-rust
  lower-to-decl/CrateItemDecl : FeatureIds Rust/CrateItemDecl -> CrateItemDecl

  [(lower-to-decl/CrateItemDecl _ (feature FeatureId))
   (feature FeatureId)]

  [(lower-to-decl/CrateItemDecl _ (struct AdtId KindedVarIds where [Rust/WhereClause ...] { Rust/FieldDecl ... }))
   (struct AdtId KindedVarIds
     where [(lower-to-decl/WhereClause Rust/WhereClause) ...]
     {(AdtId {(lower-to-decl/FieldDecl Rust/FieldDecl) ...})})]

  [(lower-to-decl/CrateItemDecl _ (enum AdtId KindedVarIds where [Rust/WhereClause ...] Rust/AdtVariants))
   (enum AdtId KindedVarIds
         where [(lower-to-decl/WhereClause Rust/WhereClause) ...]
         {(VariantId {(lower-to-decl/FieldDecl Rust/FieldDecl) ...}) ...})

   (where [(VariantId [Rust/FieldDecl ...]) ...] Rust/AdtVariants)
   ]

  [(lower-to-decl/CrateItemDecl FeatureIds  (trait TraitId [KindedVarId ...]
                                                   where [Rust/WhereClause ...]
                                                   { Rust/TraitItem ... }))
   (trait TraitId [(type Self) KindedVarId ...]
          where [(lower-to-decl/WhereClause Rust/WhereClause) ...]
          { (lower-to-decl/TraitItem FeatureIds Rust/TraitItem) ... })]

  [(lower-to-decl/CrateItemDecl FeatureIds (impl KindedVarIds TraitId[UserParameter ...] for UserTy
                                                 where [Rust/WhereClause ...]
                                                 { Rust/ImplItem ... }))
   (impl KindedVarIds (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...])
         where [(lower-to-decl/WhereClause Rust/WhereClause) ...
                (default-bound-for-in-scope-parameter FeatureIds (type (user-ty UserTy)))
                (appropriate-in-scope-goal FeatureIds KindedVarIds UserParameter) ...
                ]
         { (lower-to-decl/ImplItem FeatureIds Rust/ImplItem) ... })]

  [(lower-to-decl/CrateItemDecl _ (static StaticId KindedVarIds where [Rust/WhereClause ...] : Rust/Ty = FnBody))
   (static StaticId KindedVarIds where [(lower-to-decl/WhereClause Rust/WhereClause) ...] : (user-ty Rust/Ty) = FnBody)]

  [(lower-to-decl/CrateItemDecl _ (const ConstId KindedVarIds where [Rust/WhereClause ...] : Rust/Ty = FnBody))
   (const ConstId KindedVarIds where [(lower-to-decl/WhereClause Rust/WhereClause) ...] : (user-ty Rust/Ty) = FnBody)]

  [(lower-to-decl/CrateItemDecl FeatureIds Rust/FnDecl)
   (lower-to-decl/FnDecl FeatureIds Rust/FnDecl)]

  )

(define-metafunction formality-rust
  ;; Creates a `in-scope (K P)` goal for some parameter `P`, determining the kind of
  ;; the parameter based on the `KindedVarIds` in scope.
  appropriate-in-scope-goal : FeatureIds KindedVarIds UserParameter -> Biformula

  [(appropriate-in-scope-goal FeatureIds (_ ... (ParameterKind VarId) _ ...) VarId)
   (default-bound-for-in-scope-parameter FeatureIds (ParameterKind VarId))
   ]

  [(appropriate-in-scope-goal FeatureIds _ Ty)
   (default-bound-for-in-scope-parameter (type Ty))
   ]

  [(appropriate-in-scope-goal FeatureIds _ Lt)
   (default-bound-for-in-scope-parameter (lifetime Lt))
   ]
  )