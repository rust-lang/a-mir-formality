#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         )

(provide core-crate-decl)

(;; Defines a `CrateDecl` for (a subset of) `libcore` that can be included in various tests.
 define-term core-crate-decl

  ,(redex-let*
    formality-decl
    [
     (TraitDecl_Send (term (rust:Send (trait ((type Self)) () ()))))
     (TraitDecl_Sync (term (rust:Sync (trait ((type Self)) () ()))))
     (TraitDecl_Copy (term (rust:Copy (trait ((type Self)) () ()))))
     (TraitDecl_Drop (term (rust:Drop (trait ((type Self)) () ()))))
     (CrateDecl_core (term (core (crate (TraitDecl_Send
                                         TraitDecl_Sync
                                         TraitDecl_Copy
                                         TraitDecl_Drop
                                         )))))
     ]
    (term CrateDecl_core)
    )
  )

(module+ test
  (test-match formality-decl CrateDecl (term core-crate-decl))
  )