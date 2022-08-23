#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test

  ; Common declarations
  (redex-let*
   formality-rust

   ((; trait Foo<T> where T: Bar { }
     Rust/TraitDecl_Foo (term (trait Foo[(type T)] where [(T : Bar())] {})))

    (; trait Bar { }
     Rust/TraitDecl_Bar (term (trait Bar[] where [] {})))
    )

   (; struct S<A, B> where A: Foo<B> { }
    redex-let*
    formality-rust

    ((Rust/AdtDecl_S (term (struct S[(type A) (type B)] where [(A : Foo(B))] {})))

     (Rust/CrateDecl (term (crate C { Rust/TraitDecl_Foo
                                      Rust/TraitDecl_Bar
                                      Rust/AdtDecl_S
                                      })))
     )

    (traced '() (test-equal
                 #f
                 (term (rust:is-program-ok ([Rust/CrateDecl] C)))))
    )

   (; struct S<A, B> where A: Foo<B> { }
    ;
    ; but with implied bounds
    redex-let*
    formality-rust

    ((Rust/AdtDecl_S (term (struct S[(type A) (type B)] where [(A : Foo[B])] {})))

     (Rust/CrateDecl (term (crate C { (feature expanded-implied-bounds)
                                      Rust/TraitDecl_Foo
                                      Rust/TraitDecl_Bar
                                      Rust/AdtDecl_S
                                      })))
     )

    (traced '() (test-equal
                 #t
                 (term (rust:is-program-ok ([Rust/CrateDecl] C)))))
    )

   (; struct S<A, B> where A: Foo<B>, B: Bar { }
    redex-let*
    formality-rust

    ((Rust/AdtDecl_S (term (struct S[(type A) (type B)]
                             where [(A : Foo(B))
                                    (B : Bar())
                                    ]
                             {})))

     (Rust/CrateDecl (term (crate C { Rust/TraitDecl_Foo
                                      Rust/TraitDecl_Bar
                                      Rust/AdtDecl_S
                                      })))
     )

    (traced '() (test-equal
                 #t
                 (term (rust:is-program-ok ([Rust/CrateDecl] C)))))
    )

   (; trait Baz<A> where Self: Foo<B> { }
    ;
    ; Not OK: `B: Bar` is required.
    redex-let*
    formality-rust

    ((Rust/TraitDecl_Baz (term (trait Baz[(type A)] where [(Self : Foo[A])] {})))
     (Rust/CrateDecl (term (crate C { Rust/TraitDecl_Foo
                                      Rust/TraitDecl_Bar
                                      Rust/TraitDecl_Baz
                                      })))
     )

    (traced '() (test-equal
                 #f
                 (term (rust:is-program-ok ([Rust/CrateDecl] C)))))
    )

   (; trait Baz<A> where Self: Foo<B>, B: Bar { }
    ;
    ; OK.
    redex-let*
    formality-rust

    ((Rust/TraitDecl_Baz (term (trait Baz[(type A)]
                                      where [(Self : Foo(A))
                                             (A : Bar())
                                             ]
                                      {})))

     (Rust/CrateDecl (term (crate C (Rust/TraitDecl_Foo
                                     Rust/TraitDecl_Bar
                                     Rust/TraitDecl_Baz
                                     ))))
     )

    (traced '() (test-equal
                 #t
                 (term (rust:is-program-ok ([Rust/CrateDecl] C)))))
    )
   )
  )