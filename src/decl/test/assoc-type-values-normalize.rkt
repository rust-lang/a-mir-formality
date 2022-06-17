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

    (; impl<T: ?Sized> Iterator for (T,) { type Item = Once; }
     TraitImplDecl_Iterator_for_Once<T>_where_T:?Sized
     (term (impl ((type T)) (Iterator ((user-ty (tuple T))))
                 where ()
                 {
                  (type Item () = (user-ty (Once T)) where ())
                  })))

    (; struct Once<T> { }
     AdtDecl_Once<T:Sized>
     (term (struct Once ((type T))
             where ((T : rust:Sized()))
             ((Once ())))))

    (CrateDecl_C
     (term (C (crate [TraitDecl_Iterator_where_Item:?Sized
                      TraitImplDecl_Iterator_for_Once<T>_where_T:?Sized
                      AdtDecl_Once<T:Sized>
                      ]))))

    (CrateDecls (term [core-crate-decl
                       (C (crate [TraitDecl_Iterator_where_Item:?Sized
                                  TraitImplDecl_Iterator_for_Once<T>_where_T:?Sized
                                  AdtDecl_Once<T:Sized>
                                  ]))
                       ]))

    ]

   (redex-let*
    formality-decl
    [(Goal_eq (term ((user-ty (@ (Iterator Item) (tuple u32)))
                     ==
                     (user-ty u32))))]

    (; Cannot normalize `(T,)` to `T`
     traced '()
            (test-equal
             (term (decl:can-prove-goal CrateDecls C Goal_eq))
             #f))

    (; Can normalize `(T,)` to `Once<T>`
     traced '()
            (test-equal
             (term (decl:can-prove-goal CrateDecls C
                                        ((user-ty (@ (Iterator Item) (tuple u32)))
                                         ==
                                         (user-ty (Once u32)))))
             #t))
    )
   )
  )