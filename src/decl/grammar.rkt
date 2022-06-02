#lang racket
(require redex/reduction-semantics "../ty/grammar.rkt")
(provide formality-decl
         crate-decl-with-id
         trait-decl-id
         item-with-id
         crate-decls
         )

(define-extended-language formality-decl formality-ty
  ;; A *program* in "decl" is a set of crates (`CrateDecls`) and a current crate (`CrateId`).
  (DeclProgram ::= (CrateDecls CrateId))

  ;; Fn bodies are not defined in this layer.
  (FnBody ::= Term)

  ;; ANCHOR:Crates
  ;; Crate declarations
  (CrateDecls ::= (CrateDecl ...))
  (CrateDecl ::= (CrateId CrateContents))
  (CrateContents ::= (crate (CrateItemDecl ...)))
  (CrateItemDecl ::= FeatureDecl AdtDecl TraitDecl TraitImplDecl ConstDecl StaticDecl FnDecl)
  ;; ANCHOR_END:Crates

  ;; FeatureDecl -- indicates a feature gate is enabled on this crate
  (FeatureDecl ::= (feature FeatureId))

  ;; AdtDecl -- struct/enum/union declarations
  (AdtDecl ::= (AdtId AdtContents))
  (AdtContents ::= (AdtKind KindedVarIds WhereClauses AdtVariants))
  (AdtVariants ::= (AdtVariant ...))
  (AdtKind ::= struct enum union)
  (AdtVariant ::= (VariantId FieldDecls))

  ;; FieldDecl -- type of a field
  (FieldDecls ::= (FieldDecl ...))
  (FieldDecl ::= (FieldId Ty))

  ;; ANCHOR:Traits
  ;; TraitDecl -- trait Foo { ... }
  ;;
  ;; Unlike in Rust, the `KindedVarIds` here always include with `(type Self)` explicitly.
  (TraitDecl ::= (TraitId TraitContents))
  (TraitContents ::= (trait KindedVarIds WhereClauses TraitItems))

  ;; TraitItem --
  (TraitItems ::= (TraitItem ...))
  (TraitItem ::= FnDecl)

  ;; Trait impls
  ;;
  ;; Note that trait impls do not have names.
  (TraitImplDecl ::= (impl KindedVarIds TraitRef WhereClauses ImplItems))

  ;; Named statics
  (StaticDecl ::= (StaticId StaticContents))
  (StaticContents ::= (static KindedVarIds WhereClauses Ty FnBody))

  ;; Named constants
  (ConstDecl ::= (ConstId ConstContents))
  (ConstContents ::= (const KindedVarIds WhereClauses Ty FnBody))

  ;; ImplItem --
  (ImplItems ::= (ImplItem ...))
  (ImplItem ::= FnDecl)
  ;; ANCHOR_END:Traits

  ;; Function
  ;;
  ;; fn foo<...>(...) -> ... where ... { body }
  (FnDecl ::= (FnId FnContents))
  (FnContents ::= (fn-decl KindedVarIds Tys Ty WhereClauses FnBody))

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  ((CrateId
    TraitImplId
    ConstId
    StaticId
    VariantId
    FeatureId
    FieldId
    FnId) variable-not-otherwise-mentioned)

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
  (const ConstId
         ((ParameterKind VarId) ...)
         WhereClauses #:refers-to (shadow VarId ...)
         Ty #:refers-to (shadow VarId ...)
         FnBody #:refers-to (shadow VarId ...))
  (static ConstId
          ((ParameterKind VarId) ...)
          WhereClauses #:refers-to (shadow VarId ...)
          Ty #:refers-to (shadow VarId ...)
          FnBody #:refers-to (shadow VarId ...))
  (fn-decl FnId
           ((ParameterKind VarId) ...)
           Tys #:refers-to (shadow VarId ...)
           Ty #:refers-to (shadow VarId ...)
           WhereClauses #:refers-to (shadow VarId ...)
           FnBody #:refers-to (shadow VarId ...))
  )

(define-metafunction formality-decl
  crate-decl-with-id : CrateDecls CrateId -> CrateDecl

  ((crate-decl-with-id (_ ... (CrateId CrateContents) _ ...) CrateId)
   (CrateId CrateContents)
   )

  )


(define-metafunction formality-decl
  crate-decls : DeclProgram -> CrateDecls

  [(crate-decls (CrateDecls CrateId))
   CrateDecls
   ]

  )

(define-metafunction formality-decl
  trait-decl-id : TraitDecl -> TraitId

  ((trait-decl-id (TraitId TraitContents)) TraitId)
  )

(define-metafunction formality-decl
  ;; Find the given named item amongst all the declared crates.
  item-with-id : CrateDecls AnyId -> Term

  ((item-with-id CrateDecls AnyId)
   Term

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (_ (crate (_ ... (AnyId Term) _ ...))) CrateDecl)
   )
  )
