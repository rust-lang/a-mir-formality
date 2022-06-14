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
     (TraitDecl_Sized (term (trait rust:Sized ((type Self)) where () {})))
     (TraitDecl_Send (term (trait rust:Send ((type Self)) where () {})))
     (TraitDecl_Sync (term (trait rust:Sync ((type Self)) where () {})))
     (TraitDecl_Copy (term (trait rust:Copy ((type Self)) where () {})))
     (TraitDecl_Drop (term (trait rust:Drop ((type Self)) where () {})))
     (CrateDecl_core (term (core (crate (
                                         TraitDecl_Sized
                                         TraitDecl_Send
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