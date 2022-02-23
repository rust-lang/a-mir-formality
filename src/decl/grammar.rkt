#lang racket
(require redex/reduction-semantics racket/set "../ty/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-decl formality-ty
  ;; Crate declaration
  (CrateDecls := (CrateDecl ...))
  (CrateDecl := (crate CrateId (AdtDecls TraitDecls TraitImplDecls)))

  ;; AdtDecl -- struct/enum/union declarations
  (AdtDecls := (AdtDecl ...))
  (AdtDecl := (AdtKind AdtId KindedVarIds WhereClauses AdtVariants))
  (AdtVariants := (AdtVariant ...))
  (AdtKind := struct enum union)
  (AdtVariant := (VariantId FieldDecls))

  ;; FieldDecl -- type of a field
  (FieldDecls := (FieldDecl ...))
  (FieldDecl := (FieldId Ty))

  ;; TraitDecl -- trait Foo { ... }
  ;;
  ;; Unlike in Rust, the `KindedVarIds` here always include with `(TyKind Self)` explicitly.
  (TraitDecls := (TraitDecl ...))
  (TraitDecl := (trait TraitId KindedVarIds WhereClauses TraitItems))

  ;; TraitItem --
  (TraitItems := (TraitItem ...))
  (TraitItem := )

  ;; Trait impls
  (TraitImplDecls := (TraitImplDecl ...))
  (TraitImplDecl := (impl KindedVarIds TraitRef WhereClauses ImplItems))

  ;; ImplItem --
  (ImplItems := (ImplItem ...))
  (ImplItem := )

  ;; WhereClause -- where clause
  (WhereClauses := (WhereClause ...))
  (WhereClause :=
               (ForAll KindedVarIds WhereClause)
               (Implemented TraitRef)
               #;(Outlives (Parameter : Lt))
               #;(ProjectionEq TraitRef :: (AssociatedTyId Substitution) = Ty)
               )

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  ((VariantId
    FieldId) variable-not-otherwise-mentioned)

  #:binding-forms
  (AdtKind AdtKind
           ((ParameterKind VarId) ...)
           WhereClauses #:refers-to (shadow VarId ...)
           AdtVariants #:refers-to (shadow VarId ...))
  (trait TraitId
         ((ParameterKind VarId) ...)
         WhereClauses #:refers-to (shadow VarId ...)
         TraitItems #:refers-to (shadow VarId ...))
  (impl ((ParameterKind VarId) ...)
        TraitRef #:refers-to (shadow VarId ...)
        WhereClauses #:refers-to (shadow VarId ...)
        ImplItems #:refers-to (shadow VarId ...))
  )


(define-metafunction formality-decl
  trait-decl-id : TraitDecl -> TraitId

  ((trait-decl-id (trait TraitId KindedVarIds WhereClauses TraitItems)) TraitId)
  )