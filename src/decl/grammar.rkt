#lang racket
(require redex/reduction-semantics "../ty/grammar.rkt")
(provide formality-decl
         crate-decl-with-id
         trait-decl-id
         trait-with-id
         adt-with-id
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
  (AdtDecl ::= (AdtKind AdtId KindedVarIds where WhereClauses AdtVariants))
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
  (TraitDecl ::= (trait TraitId KindedVarIds where WhereClauses TraitItems))

  ;; TraitItem --
  (TraitItems ::= (TraitItem ...))
  (TraitItem ::= FnDecl)

  ;; Trait impls
  ;;
  ;; Note that trait impls do not have names.
  (TraitImplDecl ::= (impl KindedVarIds TraitRef where WhereClauses ImplItems))

  ;; Named statics
  (StaticDecl ::= (static StaticId KindedVarIds where WhereClauses : Ty = FnBody))

  ;; Named constants
  (ConstDecl ::= (const ConstId KindedVarIds where WhereClauses : Ty = FnBody))

  ;; ImplItem --
  (ImplItems ::= (ImplItem ...))
  (ImplItem ::= FnDecl)
  ;; ANCHOR_END:Traits

  ;; Function
  ;;
  ;; fn foo<...>(...) -> ... where ... { body }
  (FnDecl ::= (fn FnId KindedVarIds Tys -> Ty where WhereClauses FnBody))

  ;; WhereClauses (defined from ty layer) -- defines the precise syntax of
  ;; where-clauses
  (WhereClause ::=
               (∀ KindedVarIds WhereClause)
               WhereClauseAtom
               )
  (WhereClauseAtoms ::= (WhereClauseAtom ...))
  (WhereClauseAtom ::=
                   (Ty : TraitId Parameters) ; T: Debug
                   (Parameter : Lt) ; T: 'a
                   (AliasTy == Ty) ; <T as Iterator>::Item == u32
                   )

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
           where WhereClauses #:refers-to (shadow VarId ...)
           AdtVariants #:refers-to (shadow VarId ...))
  (trait TraitId
         ((ParameterKind VarId) ...)
         where WhereClauses #:refers-to (shadow VarId ...)
         TraitItems #:refers-to (shadow VarId ...))
  (impl ((ParameterKind VarId) ...)
        TraitRef #:refers-to (shadow VarId ...)
        where WhereClauses #:refers-to (shadow VarId ...)
        ImplItems #:refers-to (shadow VarId ...))
  (const ConstId
         ((ParameterKind VarId) ...)
         where WhereClauses #:refers-to (shadow VarId ...)
         : Ty #:refers-to (shadow VarId ...)
         = FnBody #:refers-to (shadow VarId ...))
  (static StaticId
          ((ParameterKind VarId) ...)
          where WhereClauses #:refers-to (shadow VarId ...)
          : Ty #:refers-to (shadow VarId ...)
          = FnBody #:refers-to (shadow VarId ...))
  (fn FnId
      ((ParameterKind VarId) ...)
      Tys #:refers-to (shadow VarId ...)
      -> Ty #:refers-to (shadow VarId ...)
      where WhereClauses #:refers-to (shadow VarId ...)
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

  ((trait-decl-id (trait TraitId _ where _ _)) TraitId)
  )

(define-metafunction formality-decl
  ;; Find the given ADT amongst all the declared crates.
  adt-with-id : CrateDecls AdtId -> AdtDecl

  [(adt-with-id CrateDecls AdtId)
   (AdtKind AdtId KindedVarIds where WhereClauses AdtVariants)

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (_ (crate (_ ... (AdtKind AdtId KindedVarIds where WhereClauses AdtVariants) _ ...))) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Find the given trait amongst all the declared crates.
  trait-with-id : CrateDecls TraitId -> TraitDecl

  [(trait-with-id CrateDecls TraitId)
   (trait TraitId KindedVarIds where WhereClauses TraitItems)

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (_ (crate (_ ... (trait TraitId KindedVarIds where WhereClauses TraitItems) _ ...))) CrateDecl)
   ]
  )
