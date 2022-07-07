#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         "where-clause.rkt"
         "field-decl.rkt"
         "trait-item.rkt"
         "impl-item.rkt"
         "fn-decl.rkt"
         )
(provide lower-to-decl/CrateItemDecl
         )

(define-metafunction formality-rust
  lower-to-decl/CrateItemDecl : Rust/CrateItemDecl -> CrateItemDecl

  [(lower-to-decl/CrateItemDecl (feature FeatureId))
   (feature FeatureId)]

  [(lower-to-decl/CrateItemDecl (struct AdtId KindedVarIds where [Rust/WhereClause ...] { Rust/FieldDecl ... }))
   (struct AdtId KindedVarIds
     where [(lower-to-decl/WhereClause Rust/WhereClause) ...]
     {(AdtId {(lower-to-decl/FieldDecl Rust/FieldDecl) ...})})]

  [(lower-to-decl/CrateItemDecl (enum AdtId KindedVarIds where [Rust/WhereClause ...] Rust/AdtVariants))
   (enum AdtId KindedVarIds
         where [(lower-to-decl/WhereClause Rust/WhereClause) ...]
         {(VariantId {(lower-to-decl/FieldDecl Rust/FieldDecl) ...}) ...})

   (where [(VariantId [Rust/FieldDecl ...]) ...] Rust/AdtVariants)
   ]

  [(lower-to-decl/CrateItemDecl (trait TraitId [KindedVarId ...]
                                       where [Rust/WhereClause ...]
                                       { Rust/TraitItem ... }))
   (trait TraitId [(type Self) KindedVarId ...]
          where [(lower-to-decl/WhereClause Rust/WhereClause) ...]
          { (lower-to-decl/TraitItem Rust/TraitItem) ... })]

  [(lower-to-decl/CrateItemDecl (impl KindedVarIds TraitId[UserParameter ...] for UserTy
                                      where [Rust/WhereClause ...]
                                      { Rust/ImplItem ... }))
   (impl KindedVarIds (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...])
         where [(lower-to-decl/WhereClause Rust/WhereClause) ...
                (well-formed (type (user-ty UserTy)))
                (appropriate-well-formed-goal KindedVarIds UserParameter) ...
                ]
         { (lower-to-decl/ImplItem Rust/ImplItem) ... })]

  [(lower-to-decl/CrateItemDecl (static StaticId KindedVarIds where [Rust/WhereClause ...] : Rust/Ty = FnBody))
   (static StaticId KindedVarIds where [(lower-to-decl/WhereClause Rust/WhereClause) ...] : (user-ty Rust/Ty) = FnBody)]

  [(lower-to-decl/CrateItemDecl (const ConstId KindedVarIds where [Rust/WhereClause ...] : Rust/Ty = FnBody))
   (const ConstId KindedVarIds where [(lower-to-decl/WhereClause Rust/WhereClause) ...] : (user-ty Rust/Ty) = FnBody)]

  [(lower-to-decl/CrateItemDecl Rust/FnDecl)
   (lower-to-decl/FnDecl Rust/FnDecl)]

  )

(define-metafunction formality-rust
  ;; Creates a `well-formed (K P)` goal for some parameter `P`, determining the kind of
  ;; the parameter based on the `KindedVarIds` in scope.
  appropriate-well-formed-goal : KindedVarIds UserParameter -> Biformula

  [(appropriate-well-formed-goal (_ ... (ParameterKind VarId) _ ...) VarId)
   (well-formed (ParameterKind VarId))
   ]

  [(appropriate-well-formed-goal _ Ty)
   (well-formed (type Ty))
   ]

  [(appropriate-well-formed-goal _ Lt)
   (well-formed (lifetime Lt))
   ]
  )