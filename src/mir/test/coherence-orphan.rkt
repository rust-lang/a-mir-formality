#lang racket
(require redex/reduction-semantics
         "../all-check.rkt"
         "../grammar.rkt"
         "../../ty/grammar.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         "../../decl/test/libcore.rkt"
         )

(module+ test
  (redex-let*
   formality-mir

   [(TraitDecl_TraitA (term (trait TraitA[(type Self)] where [] {})))
    (AdtDecl_StructA (term (struct StructA[] where [] [(StructA {})])))

    ; the various impls one might write
    (TraitImplDecl_AforA (term (impl[] (TraitA[(user-ty (StructA < >))]) where [] {})))
    ]

   (; both trait/struct in same crate
    redex-let*
    formality-mir
    [(DeclProgram (term ([core-crate-decl
                          (CrateA (crate [TraitDecl_TraitA AdtDecl_StructA TraitImplDecl_AforA]))
                          ]
                         CrateA)))]
    (traced '()
            (test-equal
             (judgment-holds (✅-Program DeclProgram))
             #t)))

   (; trait/struct in parent crate, impl in child -- error
    redex-let*
    formality-mir
    [(DeclProgram (term ([core-crate-decl
                          (CrateA (crate [TraitDecl_TraitA AdtDecl_StructA]))
                          (CrateB (crate [TraitImplDecl_AforA]))
                          ]
                         CrateB)))]
    (traced '() (test-equal
                 (judgment-holds (✅-Program DeclProgram))
                 #f)))

   (; trait in parent crate, struct/impl in child -- ok
    redex-let*
    formality-mir
    [(DeclProgram (term ([core-crate-decl
                          (CrateA (crate [TraitDecl_TraitA]))
                          (CrateB (crate [AdtDecl_StructA TraitImplDecl_AforA]))
                          ]
                         CrateB)))]
    (traced '() (test-equal
                 (judgment-holds (✅-Program DeclProgram))
                 #t)))

   (; struct in parent crate, trait/impl in child -- ok
    redex-let*
    formality-mir
    [(DeclProgram (term ([core-crate-decl
                          (CrateA (crate [AdtDecl_StructA]))
                          (CrateB (crate [TraitDecl_TraitA TraitImplDecl_AforA]))
                          ]
                         CrateB)))]
    (traced '() (test-equal
                 (judgment-holds (✅-Program DeclProgram))
                 #t)))

   )
  )