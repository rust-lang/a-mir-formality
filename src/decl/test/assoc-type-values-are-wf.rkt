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

   [(; trait Iterator { type Item: ?Sized; }
     TraitDecl_Iterator_where_Item:?Sized
     (term (trait-decl-with-bound Iterator (: (type Item) ())))
     )

    (; impl<T: ?Sized> Iterator for (T,) { type Item = MyIter<T>; }
     TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
     (term (impl ((type T)) (Iterator ((user-ty (tuple T))))
                 where ()
                 {
                  (type Item () = (user-ty (MyIter T)) where ())
                  })))

    (; struct MyIter<T> { }
     AdtDecl_MyIter<T:Sized>
     (term (struct MyIter ((type T))
             where ((T : rust:Sized()))
             ((MyIter ())))))

    (; struct MyIter<T: ?Sized> { }
     AdtDecl_MyIter<T:?Sized>
     (term (struct MyIter ((type T))
             where ()
             ((MyIter ())))))
    ]

   (; Impl supplies ?Sized, MyIter requires ?Sized: OK
    traced '()
           (decl:test-crate-decl-ok [core-crate-decl
                                     (C (crate [TraitDecl_Iterator_where_Item:?Sized
                                                TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
                                                AdtDecl_MyIter<T:?Sized>
                                                ]))
                                     ]
                                    C)
           )

   (; Impl supplies ?Sized, MyIter requires Sized: not OK
    traced '()
           (decl:test-crate-decl-not-ok [core-crate-decl
                                         (C (crate [TraitDecl_Iterator_where_Item:?Sized
                                                    TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
                                                    AdtDecl_MyIter<T:Sized>
                                                    ]))
                                         ]
                                        C)
           )
   )
  )