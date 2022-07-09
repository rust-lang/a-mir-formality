#lang racket
(require redex/reduction-semantics
         "../mir/grammar.rkt"
         "../logic/substitution.rkt"
         )
(provide formality-rust
         )

(define-extended-language formality-rust formality-mir
  ;; A *program* in "decl" is a set of crates (`CrateDecls`) and a current crate (`CrateId`).
  (Rust/Program ::= (Rust/CrateDecls CrateId))

  ;; Crate declarations
  (Rust/CrateDecls ::= (Rust/CrateDecl ...))
  (Rust/CrateDecl ::= (crate CrateId Rust/CrateItemDecls))
  (Rust/CrateItemDecls ::= { Rust/CrateItemDecl ... })
  (Rust/CrateItemDecl ::=
                      Rust/FeatureDecl
                      Rust/AdtDecl
                      Rust/TraitDecl
                      Rust/TraitImplDecl
                      Rust/StaticDecl
                      Rust/ConstDecl
                      Rust/FnDecl
                      )

  ;; FeatureDecl -- indicates a feature gate is enabled on this crate
  (Rust/FeatureDecl ::= (feature FeatureId))

  ;; TraitDecl -- trait declaration
  ;;
  ;; In this layer, the `(type Self)` parameter is NOT explicitly written.
  (Rust/TraitDecl ::= (trait TraitId KindedVarIds where Rust/WhereClauses Rust/TraitItems))

  ;; Trait item(s)
  (Rust/TraitItems ::= { Rust/TraitItem ... })
  (Rust/TraitItem ::= Rust/FnDecl Rust/AssociatedTyDecl)

  ;; Associated type declarations (in a trait)
  (Rust/AssociatedTyDecl ::= (type AssociatedTyId KindedVarIds : Rust/BoundsClause where Rust/WhereClauses))

  ;; Bounds clauses: the things an associated type must meet.
  ;;
  ;; `type Foo: Debug`           => `type Foo[] : (Debug[]) where []`
  ;; `type Foo: for<'a> Foo<'a>` => `type Foo[] : (for [(lifetime a)] (Foo[a])) where []`
  ;; `type Foo: 'a`              => `type Foo[] : (lifetime a) where []`
  (Rust/BoundsClause ::= [Rust/Bound ...])
  (Rust/Bound ::=
              (for KindedVarIds Rust/Bound)
              (TraitId UserParameters)
              (; Meant to represent `T: Iterator<Item = u32>`, but we write it
               ; `T : Iterator[] :: Item[] == u32`.
               TraitId UserParameters :: AssociatedTyId UserParameters == Rust/Ty)
              UserParameter
              )

  ;; TraitImplDecl -- an impl of a trait for a type
  (Rust/TraitImplDecl ::= (impl KindedVarIds TraitId UserParameters for UserTy where Rust/WhereClauses Rust/ImplItems))

  ;; Impl item(s)
  (Rust/ImplItems ::= { Rust/ImplItem ... })
  (Rust/ImplItem ::= Rust/FnDecl Rust/AssociatedTyValue)

  ;; Associated type value (in an impl)
  (Rust/AssociatedTyValue ::= (type AssociatedTyId KindedVarIds = Rust/Ty where Rust/WhereClauses))

  ;; Function
  (Rust/FnDecl ::= (fn FnId KindedVarIds Rust/Tys -> Rust/Ty where Rust/WhereClauses FnBody))

  ;; Named statics
  (Rust/StaticDecl ::= (static StaticId KindedVarIds where Rust/WhereClauses : Rust/Ty = FnBody))

  ;; Named constants
  (Rust/ConstDecl ::= (const ConstId KindedVarIds where Rust/WhereClauses : Rust/Ty = FnBody))

  ;; AdtDecl -- struct/enum/union declarations
  (Rust/AdtDecl ::= Rust/StructDecl Rust/EnumDecl)
  (Rust/StructDecl ::= (struct AdtId KindedVarIds where Rust/WhereClauses Rust/FieldDecls))
  (Rust/EnumDecl ::= (enum AdtId KindedVarIds where Rust/WhereClauses Rust/AdtVariants))
  (Rust/AdtVariants ::= {Rust/AdtVariant ...})
  (Rust/AdtVariant ::= (VariantId Rust/FieldDecls))

  ;; FieldDecl -- type of a field
  (Rust/FieldDecls ::= {Rust/FieldDecl ...})
  (Rust/FieldDecl ::= (FieldId : UserTy))

  ;; Pair of a kind + a parameter (e.g., `(type T)` or `(lifetime L)`)
  (KindedUserParameter ::= (ParameterKind UserParameter))

  ;; Rust-style where-clauses
  (Rust/WhereClauses ::= [Rust/WhereClause ...])
  (Rust/WhereClause ::=
                    (for KindedVarIds Rust/WhereClause)
                    Rust/WhereClauseAtom
                    )
  (Rust/WhereClauseAtoms ::= (Rust/WhereClauseAtom ...))
  (Rust/WhereClauseAtom ::=
                        (; T: Debug
                         UserTy : TraitId UserParameters)
                        (; T: 'a
                         UserParameter : UserParameter)
                        (; <T as Iterator<'a>>::Item<'a> = u32
                         ;
                         ; This is roughly the syntax proposed in rust-lang/rust#20041.
                         ;
                         ; FIXME(#80). We should support the syntax used in stable Rust.
                         < UserTy as TraitId UserParameters > :: AssociatedTyId UserParameters == UserTy)
                        )

  ;; Higher-ranked where-clauses for tests
  (Rust/WhereClauseHr ::=
                      Rust/WhereClause
                      (∀ KindedVarIds where Rust/WhereClauses Rust/WhereClause)
                      )

  ; FIXME: Unify these
  (Rust/Tys ::= UserTys)
  (Rust/Ty ::= UserTy)

  ;; Fn bodies are not defined in this layer.
  (FnBody ::= Term)
  )