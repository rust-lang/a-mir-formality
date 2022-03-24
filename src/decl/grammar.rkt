#lang racket
(require redex/reduction-semantics "../ty/grammar.rkt")
(provide formality-decl
         trait-decl-id
         item-with-id
         scalar-ty
         TyUnit
         )

(define-extended-language formality-decl formality-ty
  (DeclProgram ::= (CrateDecls CrateId))

  ;; ANCHOR:Crates
  ;; Crate declarations
  (CrateDecls ::= (CrateDecl ...))
  (CrateDecl ::= (CrateId CrateContents))
  (CrateContents ::= (crate (CrateItemDecl ...)))
  (CrateItemDecl ::= AdtDecl TraitDecl TraitImplDecl)
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

  ;; ImplItem --
  (ImplItems ::= (ImplItem ...))
  (ImplItem ::= NotYetImplemented)

  ;; WhereClause -- where clause
  (WhereClauses ::= (WhereClause ...))
  (WhereClause ::=
               (ForAll KindedVarIds WhereClause)
               (Implemented TraitRef)
               #;(Outlives (Parameter : Lt))
               #;(ProjectionEq TraitRef :: (AssociatedTyId Substitution) = Ty)
               )

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  ((CrateId
    TraitImplId
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
