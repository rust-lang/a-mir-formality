#lang racket
(require redex/reduction-semantics
         "../ty/grammar.rkt"
         "../logic/substitution.rkt"
         )
(provide formality-decl
         crate-decl-with-id
         trait-decl-id
         trait-with-id
         adt-with-id
         fn-with-id
         static-with-id
         crate-decls
         instantiate-bounds-clause
         crate-defining-trait-with-id
         crate-defining-adt-with-id
         )

(define-extended-language formality-decl formality-ty
  ;; A *program* in "decl" is a set of crates (`CrateDecls`) and a current crate (`CrateId`).
  (DeclProgram ::= (CrateDecls CrateId))

  ;; Fn bodies are not defined in this layer.
  (FnBody ::= Term)

  ;; ANCHOR:Crates
  ;; Crate declarations
  (CrateDecls ::= (CrateDecl ...))
  (CrateDecl ::= (crate CrateId CrateItemDecls))
  (CrateItemDecls ::= (CrateItemDecl ...))
  (CrateItemDecl ::= FeatureDecl AdtDecl TraitDecl TraitImplDecl ConstDecl StaticDecl FnDecl)
  ;; ANCHOR_END:Crates

  ;; FeatureDecl -- indicates a feature gate is enabled on this crate
  (FeatureDecl ::= (feature FeatureId))

  ;; AdtDecl -- struct/enum/union declarations
  (AdtDecl ::= (AdtKind AdtId KindedVarIds where Biformulas AdtVariants))
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
  (TraitDecl ::= (trait TraitId KindedVarIds where Biformulas TraitItems))

  ;; TraitItem --
  (TraitItems ::= (TraitItem ...))
  (TraitItem ::= FnDecl AssociatedTyDecl)

  ;; Associated type declarations (in a trait)
  (AssociatedTyDecl ::= (type AssociatedTyId KindedVarIds BoundsClause where Biformulas))

  ;; Bounds clause -- used in associated type declarations etc to indicate
  ;; things that must be true about the value of an associated type.
  ;;
  ;; The `VarId` is the name of the associated type. e.g., where in Rust we might write
  ;;
  ;; ```rust
  ;; trait Iterator {
  ;;     type Item: Sized;
  ;; }
  ;; ```
  ;;
  ;;                             name for the item
  ;;                                     |
  ;;                                     ▼
  ;; that becomes `(type Item() (: (type I) (I : Sized())) where ())`
  ;;                                        —————————————
  ;;                                             |
  ;;                                 represents the `: Sized` part
  ;;
  ;; This notation is kind of painful and maybe we should improve it! It'd be nice to
  ;; write `(: (Sized()))` instead.
  (BoundsClause ::= (: KindedVarId Biformulas))

  ;; Trait impls
  ;;
  ;; Note that trait impls do not have names.
  (TraitImplDecls ::= (TraitImplDecl ...))
  (TraitImplDecl ::= (impl KindedVarIds TraitRef where Biformulas ImplItems))

  ;; Named statics
  (StaticDecl ::= (static StaticId KindedVarIds where Biformulas : Ty = FnBody))

  ;; Named constants
  (ConstDecl ::= (const ConstId KindedVarIds where Biformulas : Ty = FnBody))

  ;; ImplItem --
  (ImplItems ::= (ImplItem ...))
  (ImplItem ::= FnDecl AssociatedTyValue)
  ;; ANCHOR_END:Traits

  ;; Associated type value (in an impl)
  (AssociatedTyValue ::= (type AssociatedTyId KindedVarIds = Ty where Biformulas))


  ;; Function
  ;;
  ;; fn foo<...>(...) -> ... where ... { body }
  (FnDecl ::= (fn FnId KindedVarIds Tys -> Ty where Biformulas FnBody))

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  (FeatureIds ::= [FeatureId ...])
  ((CrateId
    TraitImplId
    ConstId
    StaticId
    VariantId
    FeatureId
    FieldId) variable-not-otherwise-mentioned)

  #:binding-forms
  (AdtKind AdtKind
           ((ParameterKind VarId) ...)
           where Biformulas #:refers-to (shadow VarId ...)
           AdtVariants #:refers-to (shadow VarId ...))
  (trait TraitId
         ((ParameterKind VarId) ...)
         where Biformulas #:refers-to (shadow VarId ...)
         TraitItems #:refers-to (shadow VarId ...))
  (impl ((ParameterKind VarId) ...)
        TraitRef #:refers-to (shadow VarId ...)
        where Biformulas #:refers-to (shadow VarId ...)
        ImplItems #:refers-to (shadow VarId ...))
  (const ConstId
         ((ParameterKind VarId) ...)
         where Biformulas #:refers-to (shadow VarId ...)
         : Ty #:refers-to (shadow VarId ...)
         = FnBody #:refers-to (shadow VarId ...))
  (static StaticId
          ((ParameterKind VarId) ...)
          where Biformulas #:refers-to (shadow VarId ...)
          : Ty #:refers-to (shadow VarId ...)
          = FnBody #:refers-to (shadow VarId ...))
  (: (ParameterKind VarId) Biformulas #:refers-to (shadow VarId))
  (fn FnId
      ((ParameterKind VarId) ...)
      Tys #:refers-to (shadow VarId ...)
      -> Ty #:refers-to (shadow VarId ...)
      where Biformulas #:refers-to (shadow VarId ...)
      FnBody #:refers-to (shadow VarId ...))
  )

(define-metafunction formality-decl
  crate-decl-with-id : CrateDecls CrateId -> CrateDecl

  ((crate-decl-with-id (_ ... (crate CrateId CrateItemDecls) _ ...) CrateId)
   (crate CrateId CrateItemDecls)
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

  ((trait-decl-id (trait TraitId _ where _ _)) TraitId)
  )

(define-metafunction formality-decl
  ;; Find the given ADT amongst all the declared crates.
  adt-with-id : CrateDecls AdtId -> AdtDecl

  [(adt-with-id CrateDecls AdtId)
   (AdtKind AdtId KindedVarIds where Biformulas AdtVariants)

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (crate _ (_ ... (AdtKind AdtId KindedVarIds where Biformulas AdtVariants) _ ...)) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Find the ID of the crate that defines `AdtId`.
  crate-defining-adt-with-id : CrateDecls AdtId -> CrateId

  [(crate-defining-adt-with-id CrateDecls AdtId)
   CrateId

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (crate CrateId (_ ... (AdtKind AdtId _ where _ _) _ ...)) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Find the given trait amongst all the declared crates.
  trait-with-id : CrateDecls TraitId -> TraitDecl

  [(trait-with-id CrateDecls TraitId)
   (trait TraitId KindedVarIds where Biformulas TraitItems)

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (crate _ (_ ... (trait TraitId KindedVarIds where Biformulas TraitItems) _ ...)) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Find the ID of the crate that defines `TraitId`.
  crate-defining-trait-with-id : CrateDecls TraitId -> CrateId

  [(crate-defining-trait-with-id CrateDecls TraitId)
   CrateId

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (crate CrateId (_ ... (trait TraitId _ where _ _) _ ...)) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Given a bound like `: Sized`, 'instantiates' to apply to a given type `T`,
  ;; yielding a where clause like `T: Sized`.
  instantiate-bounds-clause : BoundsClause Parameter -> Biformulas

  [(instantiate-bounds-clause (: (ParameterKind VarId) Biformulas) Parameter)
   (apply-substitution ((VarId Parameter)) Biformulas)
   ]
  )

(define-metafunction formality-decl
  ;; Find the given function amongst all the declared crates.
  ;;
  ;; FIXME: search trait items and impl items
  fn-with-id : CrateDecls FnId -> FnDecl

  [(fn-with-id CrateDecls FnId)
   (fn FnId KindedVarIds Tys -> Ty where Biformulas FnBody)

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (crate _ (_ ... (fn FnId KindedVarIds Tys -> Ty where Biformulas FnBody) _ ...)) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Find the given static variable amongst all the declared crates.
  static-with-id : CrateDecls StaticId -> StaticDecl

  [(static-with-id CrateDecls StaticId)
   (static StaticId KindedVarIds where Biformulas : Ty = FnBody)

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (crate _ (_ ... (static StaticId KindedVarIds where Biformulas : Ty = FnBody) _ ...)) CrateDecl)
   ]
  )
