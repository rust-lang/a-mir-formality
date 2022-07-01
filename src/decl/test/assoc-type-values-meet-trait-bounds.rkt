#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         "libcore.rkt"
         )

(module+ test
  (define-metafunction formality-decl
    trait-decl-with-bound : TraitId BoundsClause -> TraitDecl

    [(trait-decl-with-bound TraitId BoundsClause)
     (trait TraitId ((type Self)) where ()
            {
             (type Item () BoundsClause where ())
             })
     ]
    )

  (redex-let*
   formality-decl

   [(; trait Iterator { type Item: Sized; }
     TraitDecl_Iterator_where_Item:Sized
     (term (trait-decl-with-bound Iterator (: (type Item) ((Item : rust:Sized ())))))
     )

    (; trait Iterator { type Item: ?Sized; }
     TraitDecl_Iterator_where_Item:?Sized
     (term (trait-decl-with-bound Iterator (: (type Item) ())))
     )

    (; struct MyIter<T: ?Sized> { }
     AdtDecl_MyIter (term (struct MyIter ((type T)) where () ((MyIter ())))))

    (; impl<T: Sized> Iterator for MyIter<T> { type Item = T; }
     TraitImplDecl_Iterator_for_MyIter<T>_where_T:Sized
     (term (impl ((type T)) Iterator () for (MyIter T)
                 where ((T : rust:Sized ()))
                 {
                  (type Item () = T where ())
                  })))

    (; impl<T: Sized> Iterator for MyIter<T> { type Item = T; }
     TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
     (term (impl ((type T)) Iterator() for (MyIter T)
                 where ()
                 {
                  (type Item () = T where ())
                  })))
    ]

   (; Trait requires Sized, impl supplies Sized: OK
    traced '()
           (decl:test-crate-decl-ok [core-crate-decl
                                     (C (crate [AdtDecl_MyIter
                                                TraitDecl_Iterator_where_Item:Sized
                                                TraitImplDecl_Iterator_for_MyIter<T>_where_T:Sized
                                                ]))
                                     ]
                                    C)
           )

   (; Trait requires ?Sized, impl supplies Sized: OK
    traced '()
           (decl:test-crate-decl-ok [core-crate-decl
                                     (C (crate [AdtDecl_MyIter
                                                TraitDecl_Iterator_where_Item:?Sized
                                                TraitImplDecl_Iterator_for_MyIter<T>_where_T:Sized
                                                ]))
                                     ]
                                    C)
           )

   (; Trait requires Sized, impl supplies ?Sized: not OK
    traced '()
           (decl:test-crate-decl-not-ok [core-crate-decl
                                         (C (crate [AdtDecl_MyIter
                                                    TraitDecl_Iterator_where_Item:Sized
                                                    TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
                                                    ]))
                                         ]
                                        C)
           )

   (; Trait requires ?Sized, impl supplies ?Sized: OK
    traced '()
           (decl:test-crate-decl-ok [core-crate-decl
                                     (C (crate [AdtDecl_MyIter
                                                TraitDecl_Iterator_where_Item:?Sized
                                                TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
                                                ]))
                                     ]
                                    C)
           )

   (test-equal #t #t)
   )
  )