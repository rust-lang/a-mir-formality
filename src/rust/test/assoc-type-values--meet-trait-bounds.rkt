#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (define-metafunction formality-rust
    trait-decl-with-bound : TraitId Rust/BoundsClause -> Rust/TraitDecl

    [(trait-decl-with-bound TraitId Rust/BoundsClause)
     (trait TraitId[] where ()
            {
             (type Item () : Rust/BoundsClause where [])
             })
     ]
    )

  (redex-let*
   formality-rust

   [(; trait Iterator { type Item: Sized; }
     Rust/TraitDecl_Iterator_where_Item:Sized
     (term (trait-decl-with-bound Iterator [(core:Sized[])]))
     )

    (; trait Iterator { type Item: ?Sized; }
     Rust/TraitDecl_Iterator_where_Item:?Sized
     (term (trait-decl-with-bound Iterator []))
     )

    (; struct MyIter<T: ?Sized> { }
     Rust/AdtDecl_MyIter (term (struct MyIter ((type T)) where () {})))

    (; impl<T: Sized> Iterator for MyIter<T> { type Item = T; }
     Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:Sized
     (term (impl[(type T)] Iterator[] for (MyIter < T >)
                where [(T : core:Sized[])]
                {
                 (type Item[] = T where [])
                 })))

    (; impl<T: Sized> Iterator for MyIter<T> { type Item = T; }
     Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
     (term (impl[(type T)] Iterator[] for (MyIter < T >)
                where []
                {
                 (type Item () = T where ())
                 })))
    ]

   (; Trait requires Sized, impl supplies Sized: OK
    traced '()
           (test-equal
            (term (rust:is-program-ok ([libcore
                                        (crate C { Rust/AdtDecl_MyIter
                                                   Rust/TraitDecl_Iterator_where_Item:Sized
                                                   Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:Sized
                                                   })
                                        ] C)))
            #t))

   (; Trait requires ?Sized, impl supplies Sized: OK
    traced '()
           (test-equal
            (term (rust:is-program-ok ([libcore
                                        (crate C { Rust/AdtDecl_MyIter
                                                   Rust/TraitDecl_Iterator_where_Item:?Sized
                                                   Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:Sized
                                                   })
                                        ]
                                       C)))
            #t))

   (; Trait requires Sized, impl supplies ?Sized: not OK
    traced '()
           (test-equal
            (term (rust:is-program-ok ([libcore
                                        (crate C { Rust/AdtDecl_MyIter
                                                   Rust/TraitDecl_Iterator_where_Item:Sized
                                                   Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
                                                   })
                                        ]
                                       C)))
            #f
            ))

   (; Trait requires ?Sized, impl supplies ?Sized: OK
    traced '()
           (test-equal
            (term (rust:is-program-ok ([libcore
                                        (crate C { Rust/AdtDecl_MyIter
                                                   Rust/TraitDecl_Iterator_where_Item:?Sized
                                                   Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Sized
                                                   })
                                        ]
                                       C)))
            #t
            ))

   (test-equal #t #t)
   )
  )