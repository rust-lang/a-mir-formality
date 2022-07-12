#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(; trait Iterator { type Item; }
     Rust/TraitDecl_Iterator_where_Item:?Copy
     (term (trait Iterator[] where []
                  {
                   (type Item () : [] where [])
                   }))
     )

    (; impl<T> Iterator for (T,) { type Item = MyIter<T>; }
     Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Copy
     (term (impl[(type T)] Iterator[] for (tuple T)
                where []
                {
                 (type Item[] = (MyIter < T >) where [])
                 })))

    (; struct MyIter<T: Copy> { }
     Rust/AdtDecl_MyIter<T:Copy>
     (term (struct MyIter[(type T)]
             where [(T : core:Copy[])]
             {})))

    (; struct MyIter<T: ?Copy> { }
     Rust/AdtDecl_MyIter<T:?Copy>
     (term (struct MyIter[(type T)]
             where []
             {})))
    ]

   (; Impl supplies ?Copy, MyIter requires ?Copy: OK
    traced '()
           (test-equal
            (term (rust:is-program-ok ([libcore
                                        (crate C { Rust/TraitDecl_Iterator_where_Item:?Copy
                                                   Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Copy
                                                   Rust/AdtDecl_MyIter<T:?Copy>
                                                   })
                                        ]
                                       C)))
            #t)
           )

   (; Impl supplies ?Copy, MyIter requires Sized: not OK
    traced '()
           (test-equal
            (term (rust:is-program-ok ([libcore
                                        (crate C { Rust/TraitDecl_Iterator_where_Item:?Copy
                                                   Rust/TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Copy
                                                   Rust/AdtDecl_MyIter<T:Copy>
                                                   })
                                        ]
                                       C)))
            #f
            ))
   )
  )