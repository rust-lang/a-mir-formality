#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../ty/user-ty.rkt"
         )
(provide rust→decl/Program
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

  [(rust→decl/WhereClause (< UserTy as TraitId UserParameters > :: AssociatedTyId UserParameters = UserTy))
   ((user-ty < UserTy as TraitId UserParameters > :: AssociatedTyId UserParameters) == (user-ty UserTy))]

  )