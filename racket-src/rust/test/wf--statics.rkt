#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(; struct Foo { }
     Rust/AdtDecl_Foo (term (struct Foo[] where [] {})))
    ]

   (; test that Send check fails
    ;
    ; FIXME(#26)  when we support auto traits, we'll need a neg impl here
    redex-let*
    formality-rust
    [(; static S: Foo = ...;
      Rust/StaticDecl (term (static S[] where [] : (Foo < >) = {trusted-fn-body})))

     (Rust/CrateDecl (term (crate TheCrate { Rust/AdtDecl_Foo
                                             Rust/StaticDecl
                                             })))
     ]

    (traced '()
            (test-equal
             (term (rust:is-program-ok ([libcore Rust/CrateDecl] TheCrate)))
             #f
             ))
    )

   (; test that Sync check succeeds when an impl is present
    ;
    ; FIXME(#26)  when we support auto traits, we won't need an impl here
    redex-let*
    formality-rust
    [(; static S: Foo = ...;
      Rust/StaticDecl (term (static S[] where [] : (Foo < >) = {trusted-fn-body})))
     (; impl Send for Foo
      Rust/TraitImplDecl_Sync (term (impl[(type T)] core:Sync[] for (Foo < >) where [] { })))
     (Rust/CrateDecl (term (crate TheCrate { Rust/AdtDecl_Foo
                                             Rust/StaticDecl
                                             Rust/TraitImplDecl_Sync
                                             })))
     ]

    (traced '()
            (test-equal
             (term (rust:is-program-ok ([libcore Rust/CrateDecl] TheCrate)))
             #t
             ))
    )
   )
  )
