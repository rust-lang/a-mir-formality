#lang racket
(require redex/reduction-semantics "../ty/grammar.rkt")
(provide formality-decl
         trait-decl-id
         item-with-id
         scalar-ty
         TyUnit
         )

(define-extended-language formality-decl formality-ty
  ;; A *program* in "decl" is a set of crates (`CrateDecls`) and a current crate (`CrateId`).
  (DeclProgram ::= (CrateDecls CrateId))

  ;; ANCHOR:Crates
  ;; Crate declarations
  (CrateDecls ::= (CrateDecl ...))
  (CrateDecl ::= (CrateId CrateContents))
  (CrateContents ::= (crate (CrateItemDecl ...)))
  (CrateItemDecl ::= AdtDecl TraitDecl TraitImplDecl ConstDecl)
  ;; ANCHOR_END:Crates

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
  ;; Unlike in Rust, the `KindedVarIds` here always include with `(TyKind Self)` explicitly.
  (TraitDecl ::= (TraitId TraitContents))
  (TraitContents ::= (trait KindedVarIds WhereClauses TraitItems))

  ;; TraitItem --
  (TraitItems ::= (TraitItem ...))
  (TraitItem ::= NotYetImplemented)

  ;; Trait impls
  ;;
  ;; Note that trait impls do not have names.
  (TraitImplDecl ::= (impl KindedVarIds TraitRef WhereClauses ImplItems))

  ;; Named constants
  (ConstDecl ::= (ConstId ConstContents))
  (ConstContents ::= (const KindedVarIds WhereClauses Ty))

  ;; ImplItem --
  (ImplItems ::= (ImplItem ...))
  (ImplItem ::= NotYetImplemented)
  ;; ANCHOR_END:Traits

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  ((CrateId
    TraitImplId
    ConstId
    VariantId
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
  (const ConstId
         ((ParameterKind VarId) ...)
         WhereClauses #:refers-to (shadow VarId ...)
         Ty #:refers-to (shadow VarId ...))
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
