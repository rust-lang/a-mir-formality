#lang racket
(require redex/reduction-semantics
         "grammar.rkt")
(provide trait-decl-with-id)

(define-metafunction formality-decl
  ;; Find the given trait amongst all the declared crates.
  trait-decl-with-id : CrateDecls TraitId -> TraitDecl

  ((trait-decl-with-id CrateDecls TraitId)
   TraitDecl

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (CrateId (crate (_ ... TraitDecl _ ...))) CrateDecl)
   (where TraitId (trait-decl-id TraitDecl))
   )
  )