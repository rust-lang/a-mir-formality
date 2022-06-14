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
  (redex-let*
   formality-decl

   [(; trait Iterator { type Item: Sized; }
     TraitDecl_Iterator (term (trait Iterator ((type Self)) where ()
                                     {
                                      (type Item () (: (type Item) ((Item : rust:Sized ()))) where ())
                                      })))

    (; struct MyIter<T> { }
     AdtDecl_MyIter (term (struct MyIter ((type T)) where ((T : rust:Sized ())) ((MyIter ())))))

    (; impl<T> Iterator for MyIter<T> { type Item = T; }
     TraitImplDecl_MyIter_Iterator (term (impl ((type T)) (Iterator ((user-ty (MyIter T)))) where ()
                                               {
                                                (type Item () (: (type Item) ((Item : rust:Sized ()))) = T where ())
                                                })))

    ]

   (; test that WF checks fail if `T: Debug` is missing
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

  )