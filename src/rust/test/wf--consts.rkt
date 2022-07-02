#lang racket
(require redex/reduction-semantics
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  (current-traced-metafunctions '(relate/one compare/one/substituted equate/one/substituted))
  (redex-let*
   formality-rust

   [(; trait Debug { }
     Rust/TraitDecl (term (trait Debug[] where [] {})))
    (; struct Foo<T: Debug> { }
     Rust/AdtDecl_Foo (term (struct Foo[(type T)] where [(T : Debug[])] {})))
    ]

   (; test that WF checks fail if `T: Debug` is missing
    redex-let*
    formality-rust
    [
     (; const BROKEN<T>: Foo<T>;
      Rust/ConstDecl_broken (term (const BROKEN[(type T)] where [] : (Foo < T >) = trusted-fn-body)))
     (Rust/CrateDecl (term (crate TheCrate { Rust/TraitDecl Rust/AdtDecl_Foo Rust/ConstDecl_broken })))
     ]

    (traced '()
            (test-equal (term (rust:is-program-ok ([Rust/CrateDecl] TheCrate)))
                        #f)))

   (; test that WF checks succeed if `T: Debug` is present
    redex-let*
    formality-rust
    [
     (; const OK<T: Debug>: Foo<T>;
      Rust/ConstDecl_ok (term (const OK[(type T)] where [(T : Debug[])] : (Foo < T >) = trusted-fn-body)))
     (Rust/CrateDecl (term (crate TheCrate { Rust/TraitDecl Rust/AdtDecl_Foo Rust/ConstDecl_ok })))
     ]

    (traced '()
            (test-equal (term (rust:is-program-ok ([Rust/CrateDecl] TheCrate)))
                        #t)))

   )

  )
