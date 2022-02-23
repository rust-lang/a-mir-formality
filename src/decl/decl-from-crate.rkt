#lang racket
(require redex/reduction-semantics
         "grammar.rkt")
(provide trait-decl-with-id-from-crate)

(define-metafunction formality-decl
  trait-decl-with-id-from-crate : CrateDecl TraitId -> TraitDecl

  ((trait-decl-with-id-from-crate (crate CrateId (AdtDecls TraitDecls TraitImplDecls)) TraitId)
   TraitDecl

   (where (_ ... TraitDecl _ ...) TraitDecls)
   (where TraitId (trait-decl-id TraitDecl))
   )
  )