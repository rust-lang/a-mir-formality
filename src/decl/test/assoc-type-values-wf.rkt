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

   [(; trait Iterator { type Item; }
     TraitDecl_Iterator_where_Item:?Copy
     (term (trait-decl-with-bound Iterator (: (type Item) ())))
     )

    (; impl<T> Iterator for (T,) { type Item = MyIter<T>; }
     TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Copy
     (term (impl ((type T)) (Iterator ((user-ty (tuple T))))
                 where ()
                 {
                  (type Item () = (user-ty (MyIter T)) where ())
                  })))

    (; struct MyIter<T: Copy> { }
     AdtDecl_MyIter<T:Copy>
     (term (struct MyIter ((type T))
             where ((T : rust:Copy()))
             ((MyIter ())))))

    (; struct MyIter<T: ?Copy> { }
     AdtDecl_MyIter<T:?Copy>
     (term (struct MyIter ((type T))
             where ()
             ((MyIter ())))))
    ]

   #;(; Impl supplies ?Copy, MyIter requires ?Copy: OK XXX
      traced '()
             (test-equal
              (term (decl:is-crate-ok [core-crate-decl
                                       (C (crate [TraitDecl_Iterator_where_Item:?Copy
                                                  TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Copy
                                                  AdtDecl_MyIter<T:?Copy>
                                                  ]))
                                       ]
                                      C))
              #t)
             )

   (; Impl supplies ?Copy, MyIter requires Sized: not OK
    ;
    ; FIXME -- this currently succeeds because
    ;
    ; - we can show that (@ (Iterator Item) (T,)) is WF (that requires the impl to apply, and it does)
    ; - we can show that it normalizes to Adt<T>
    ;
    ; In other words, it's a kind of "self-supporting impl" problem! If we are able to show that
    ; the type X is WF because we can find something that normalizes to it, that's a problem.
    traced '(prove program-rules)
           (test-equal
            (term (decl:is-crate-ok [core-crate-decl
                                     (C (crate [TraitDecl_Iterator_where_Item:?Copy
                                                TraitImplDecl_Iterator_for_MyIter<T>_where_T:?Copy
                                                AdtDecl_MyIter<T:Copy>
                                                ]))
                                     ]
                                    C))
            #f
            ))
   )
  )