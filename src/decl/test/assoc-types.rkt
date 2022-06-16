#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         "libcore.rkt"
         )

(module+ test
  (current-traced-metafunctions '(relate/one compare/one/substituted equate/one/substituted))


  (;; positive test: trait requests Sized, and impl supplies it
   redex-let*
   formality-decl

   [(; trait Iterator { type Item: Sized; }
     TraitDecl_Iterator (term (trait Iterator ((type Self)) where ()
                                     {
                                      (type Item () (: (type Item) ((Item : rust:Sized ()))) where ())
                                      })))

    (; struct MyIter<T: ?Sized> { }
     AdtDecl_MyIter (term (struct MyIter ((type T)) where () ((MyIter ())))))

    (; impl<T: Sized> Iterator for MyIter<T> { type Item = T; }
     TraitImplDecl_MyIter_Iterator (term (impl ((type T)) (Iterator ((user-ty (MyIter T))))
                                               where ((T : rust:Sized ()))
                                               {
                                                (type Item () = T where ())
                                                })))

    ]

   (; test that the above crate is WF
    redex-let*
    formality-decl
    [
     (CrateDecl (term (C (crate (TraitDecl_Iterator
                                 AdtDecl_MyIter
                                 TraitImplDecl_MyIter_Iterator
                                 )))))
     ]

    (traced '()
            (decl:test-crate-decl-ok (CrateDecl core-crate-decl) C)
            )
    )
   )

  (;; negative test: the impl doesn't supply Sized
   redex-let*
   formality-decl

   [(; trait Iterator { type Item: Sized; }
     TraitDecl_Iterator (term (trait Iterator ((type Self)) where ()
                                     {
                                      (type Item () (: (type Item) ((Item : rust:Sized ()))) where ())
                                      })))

    (; struct MyIter<T: ?Sized> { }
     AdtDecl_MyIter (term (struct MyIter ((type T)) where () ((MyIter ())))))

    (; impl<T: ?Sized> Iterator for MyIter<T> { type Item = T; }
     TraitImplDecl_MyIter_Iterator (term (impl ((type T)) (Iterator ((user-ty (MyIter T))))
                                               where () ; missing where clause here
                                               {
                                                (type Item () = T where ())
                                                })))

    ]

   (; above crate is not valid
    redex-let*
    formality-decl
    [
     (CrateDecl (term (C (crate (TraitDecl_Iterator
                                 AdtDecl_MyIter
                                 TraitImplDecl_MyIter_Iterator
                                 )))))
     ]

    (traced '()
            (decl:test-crate-decl-not-ok (CrateDecl core-crate-decl) C)
            )
    )
   )

  )