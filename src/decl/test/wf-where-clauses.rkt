#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt")

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test

  ; Common declarations
  (redex-let*
   formality-decl

   ((; trait Foo<T> where T: Bar { }
     TraitDecl_Foo (term (trait Foo ((type Self) (type T)) ((T : Bar())) ())))

    (; trait Bar { }
     TraitDecl_Bar (term (trait Bar ((type Self)) () ())))
    )

   (; struct S<A, B> where A: Foo<B> { }
    redex-let*
    formality-decl

    ((AdtDecl_S (term (struct S ((type A) (type B))
                        ((A : Foo(B)))
                        ((S ())))))

     (CrateDecl (term (C (crate (TraitDecl_Foo
                                 TraitDecl_Bar
                                 AdtDecl_S
                                 )))))
     )

    (traced '() (decl:test-crate-decl-not-ok (CrateDecl) C))
    )

   (; struct S<A, B> where A: Foo<B> { }
    ;
    ; but with implied bounds
    redex-let*
    formality-decl

    ((AdtDecl_S (term (struct S ((type A) (type B))
                        ((A : Foo(B)))
                        ((S ())))))

     (CrateDecl (term (C (crate ((feature expanded-implied-bounds)
                                 TraitDecl_Foo
                                 TraitDecl_Bar
                                 AdtDecl_S
                                 )))))
     )

    (traced '() (decl:test-crate-decl-ok (CrateDecl) C))
    )

   (; struct S<A, B> where A: Foo<B>, B: Bar { }
    redex-let*
    formality-decl

    ((AdtDecl_S (term (struct S ((type A) (type B))
                        ((A : Foo(B))
                         (B : Bar())
                         )
                        ((S ())))))

     (CrateDecl (term (C (crate (TraitDecl_Foo
                                 TraitDecl_Bar
                                 AdtDecl_S
                                 )))))
     )

    (traced '() (decl:test-crate-decl-ok (CrateDecl) C))
    )

   (; trait Baz<A> where Self: Foo<B> { }
    ;
    ; Not OK: `B: Bar` is required.
    redex-let*
    formality-decl

    ((TraitDecl_Baz (term (trait Baz ((type Self) (type A))
                                 ((Self : Foo(A))
                                  )
                                 ())))
     (CrateDecl (term (C (crate (TraitDecl_Foo
                                 TraitDecl_Bar
                                 TraitDecl_Baz
                                 )))))
     )

    (traced '() (decl:test-crate-decl-not-ok (CrateDecl) C))
    )

   (; trait Baz<A> where Self: Foo<B>, B: Bar { }
    ;
    ; OK.
    redex-let*
    formality-decl

    ((TraitDecl_Baz (term (trait Baz ((type Self) (type A))
                                 ((Self : Foo(A))
                                  (A : Bar())
                                  )
                                 ())))

     (CrateDecl (term (C (crate (TraitDecl_Foo
                                 TraitDecl_Bar
                                 TraitDecl_Baz
                                 )))))
     )

    (traced '() (decl:test-crate-decl-ok (CrateDecl) C))
    )
   )

  )