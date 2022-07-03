#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../ty/user-ty.rkt"
         )
(provide rust→decl/Program
         rust→decl/WhereClause
         )

(define-metafunction formality-rust
  rust→decl/Program : Rust/Program -> DeclProgram

  [(rust→decl/Program ([Rust/CrateDecl ...] CrateId))
   ([(rust→decl/Crate Rust/CrateDecl) ...] CrateId)
   ]
  )

(define-metafunction formality-rust
  rust→decl/Crate : Rust/CrateDecl -> CrateDecl

  [(rust→decl/Crate (crate CrateId (Rust/CrateItemDecl ...)))
   (crate CrateId ((rust→decl/CrateItemDecl Rust/CrateItemDecl) ...))
   ]
  )

(define-metafunction formality-rust
  rust→decl/CrateItemDecl : Rust/CrateItemDecl -> CrateItemDecl

  [(rust→decl/CrateItemDecl (feature FeatureId))
   (feature FeatureId)]

  [(rust→decl/CrateItemDecl (struct AdtId KindedVarIds where [Rust/WhereClause ...] { Rust/FieldDecl ... }))
   (struct AdtId KindedVarIds
     where [(rust→decl/WhereClause Rust/WhereClause) ...]
     {(AdtId {(rust→decl/FieldDecl Rust/FieldDecl) ...})})]

  [(rust→decl/CrateItemDecl (enum AdtId KindedVarIds where [Rust/WhereClause ...] Rust/AdtVariants))
   (enum AdtId KindedVarIds
         where [(rust→decl/WhereClause Rust/WhereClause) ...]
         {(VariantId {(rust→decl/FieldDecl Rust/FieldDecl) ...}) ...})

   (where [(VariantId [Rust/FieldDecl ...]) ...] Rust/AdtVariants)
   ]

  [(rust→decl/CrateItemDecl (trait TraitId [KindedVarId ...]
                                   where [Rust/WhereClause ...]
                                   { Rust/TraitItem ... }))
   (trait TraitId [(type Self) KindedVarId ...]
          where [(rust→decl/WhereClause Rust/WhereClause) ...]
          { (rust→decl/TraitItem Rust/TraitItem) ... })]

  [(rust→decl/CrateItemDecl (impl KindedVarIds TraitId[UserParameter ...] for UserTy
                                  where [Rust/WhereClause ...]
                                  { Rust/ImplItem ... }))
   (impl KindedVarIds (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...])
         where [(rust→decl/WhereClause Rust/WhereClause) ...]
         { (rust→decl/ImplItem Rust/ImplItem) ... })]

  [(rust→decl/CrateItemDecl (static StaticId KindedVarIds where [Rust/WhereClause ...] : Rust/Ty = FnBody))
   (static StaticId KindedVarIds where [(rust→decl/WhereClause Rust/WhereClause) ...] : (user-ty Rust/Ty) = FnBody)]

  [(rust→decl/CrateItemDecl (const ConstId KindedVarIds where [Rust/WhereClause ...] : Rust/Ty = FnBody))
   (const ConstId KindedVarIds where [(rust→decl/WhereClause Rust/WhereClause) ...] : (user-ty Rust/Ty) = FnBody)]

  )

(define-metafunction formality-rust
  rust→decl/ImplItem : Rust/ImplItem -> ImplItem

  [(rust→decl/ImplItem (type AssociatedTyId KindedVarIds = Rust/Ty where [Rust/WhereClause ...]))
   (type AssociatedTyId KindedVarIds = (user-ty Rust/Ty) where [(rust→decl/WhereClause Rust/WhereClause) ...])]

  [(rust→decl/ImplItem Rust/FnDecl)
   (rust→decl/FnDecl Rust/FnDecl)]

  )

(define-metafunction formality-rust
  rust→decl/TraitItem : Rust/TraitItem -> TraitItem

  [(rust→decl/TraitItem Rust/AssociatedTyDecl)
   (rust→decl/AssociatedTyDecl Rust/AssociatedTyDecl)]

  [(rust→decl/TraitItem Rust/FnDecl)
   (rust→decl/FnDecl Rust/FnDecl)]

  )

(define-metafunction formality-rust
  rust→decl/FnDecl : Rust/AssociatedTyDecl -> AssociatedTyDecl

  [(rust→decl/FnDecl (type AssociatedTyId KindedVarIds : Rust/BoundsClause where [Rust/WhereClause ...]))
   (type AssociatedTyId KindedVarIds
         (rust→decl/BoundsClause Rust/BoundsClause)
         where [(rust→decl/WhereClause Rust/WhereClause) ...])]

  )

(define-metafunction formality-rust
  rust→decl/AssociatedTyDecl : Rust/AssociatedTyDecl -> AssociatedTyDecl

  [(rust→decl/AssociatedTyDecl (type AssociatedTyId KindedVarIds : Rust/BoundsClause where [Rust/WhereClause ...]))
   (type AssociatedTyId KindedVarIds
         (rust→decl/BoundsClause Rust/BoundsClause)
         where [(rust→decl/WhereClause Rust/WhereClause) ...])]

  )

(define-metafunction formality-rust
  rust→decl/BoundsClause : Rust/BoundsClause -> BoundsClause

  [(rust→decl/BoundsClause [Rust/Bound ...])
   (: (type VarId_fresh) [(rust→decl/WhereClause Rust/WhereClause) ...])
   ; we have to introduce a fresh variable VarId to become the bound variable that is present
   ; in decl syntax, but not in rust
   (where/error VarId_fresh ,(variable-not-in (term [Rust/Bound ...]) 'X))
   (where/error [Rust/WhereClause ...] [(instantiate-rust-bound VarId_fresh Rust/Bound) ...])]
  )

(define-metafunction formality-rust
  ;; Convert a Rust/Bound, applied to a type named VarId (which does not appear
  ;; in Rust/Bound) to a Rust/WhereClause.
  instantiate-rust-bound : VarId Rust/Bound -> Rust/WhereClause

  [(instantiate-rust-bound VarId (for KindedVarIds Rust/Bound))
   (for KindedVarIds (instantiate-rust-bound VarId Rust/Bound))
   ]

  [(instantiate-rust-bound VarId (TraitId UserParameters))
   (VarId : TraitId UserParameters)
   ]

  [(instantiate-rust-bound VarId (TraitId UserParameters_t :: AssociatedTyId UserParameters_i == Rust/Ty))
   (< VarId as TraitId UserParameters_t > :: AssociatedTyId UserParameters_i == Rust/Ty)
   ]

  [(instantiate-rust-bound VarId KindedUserParameter)
   ((type VarId) : KindedUserParameter)
   ]

  )

(define-metafunction formality-rust
  rust→decl/FieldDecl : Rust/FieldDecl -> FieldDecl

  [(rust→decl/FieldDecl (FieldId : UserTy))
   (FieldId (user-ty UserTy))]

  )

(define-metafunction formality-rust
  rust→decl/WhereClause : Rust/WhereClause -> WhereClause

  [(rust→decl/WhereClause (for KindedVarIds Rust/WhereClause))
   (∀ KindedVarIds (rust→decl/WhereClause Rust/WhereClause))]

  [(rust→decl/WhereClause (UserTy : TraitId [UserParameter ...]))
   ((user-ty UserTy) : TraitId [(user-parameter UserParameter) ...])]

  [(rust→decl/WhereClause ((ParameterKind_1 UserParameter_1) : (ParameterKind_2 UserParameter_2)))
   ((ParameterKind_1 (user-parameter UserParameter_1)) : (ParameterKind_2 (user-parameter UserParameter_2)))]

  [(rust→decl/WhereClause (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2 == UserTy_v))
   ((user-ty (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2)) == (user-ty UserTy_v))]

  )