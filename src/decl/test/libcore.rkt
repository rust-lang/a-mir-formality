#lang racket
(require redex/reduction-semantics
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         )

(provide core-crate-decl)

(;; Defines a `CrateDecl` for (a subset of) `libcore` that can be included in various tests.
 define-term core-crate-decl

  ,(redex-let*
    formality-decl
    [
     (TraitDecl_Sized (term (trait core:Sized ((type Self)) where () {})))
     (TraitDecl_Send (term (trait core:Send ((type Self)) where () {})))
     (TraitDecl_Sync (term (trait core:Sync ((type Self)) where () {})))
     (TraitDecl_Copy (term (trait core:Copy ((type Self)) where () {})))
     (TraitDecl_Drop (term (trait core:Drop ((type Self)) where () {})))

     (TraitImplDecl_Copy_for_u32 (term (impl[] (core:Copy[(user-ty u32)]) where [] {})))
     (TraitImplDecl_Copy_for_i32 (term (impl[] (core:Copy[(user-ty i32)]) where [] {})))

     (CrateDecl_core (term (crate core {TraitDecl_Sized
                                        TraitDecl_Send
                                        TraitDecl_Sync
                                        TraitDecl_Copy
                                        TraitDecl_Drop
                                        TraitImplDecl_Copy_for_u32
                                        TraitImplDecl_Copy_for_i32
                                        })))
     ]
    (term CrateDecl_core)
    )
  )

(module+ test
  (test-match formality-decl CrateDecl (term core-crate-decl))
  )