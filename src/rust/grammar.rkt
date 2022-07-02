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
  (Rust/CrateDecl ::= (crate CrateId {Rust/CrateItemDecl ...}))
  (Rust/CrateItemDecl ::=
                      Rust/FeatureDecl
                      Rust/AdtDecl
                      )

  ;; FeatureDecl -- indicates a feature gate is enabled on this crate
  (Rust/FeatureDecl ::= (feature FeatureId))

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
                         KindedUserParameter : KindedUserParameter)
                        (; <T as Iterator<'a>>::Item<'a> = u32
                         < UserTy as TraitId UserParameters > :: AssociatedTyId UserParameters = UserTy)
                        )

  ;; Fn bodies are not defined in this layer.
  (FnBody ::= Term)
  )