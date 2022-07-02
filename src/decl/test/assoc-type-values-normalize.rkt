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

   [(; trait Iterator { type Item; }
     TraitDecl_Iterator_where_Item
     (term (trait Iterator[(type Self)] where ()
                  {
                   (type Item () (: (type Self) []) where ())
                   }))
     )

    (; impl<T> Iterator for (T,) { type Item = Once; }
     TraitImplDecl_Iterator_for_Once<T>
     (term (impl ((type T)) (Iterator ((user-ty (tuple T))))
                 where ()
                 {
                  (type Item () = (user-ty (Once < T >)) where ())
                  })))

    (; struct Once<T> { }
     AdtDecl_Once<T>
     (term (struct Once ((type T))
             where ()
             ((Once ())))))

    (; impl<T> Copy for Once<T> where T: Copy { }
     TraitImplDecl_Copy_for_Once<T>
     (term (impl[(type T)] (core:Copy[(user-ty (Once < T >))])
                where [(T : core:Copy[])]
                {})))

    (CrateDecl_C
     (term (crate C [TraitDecl_Iterator_where_Item
                      TraitImplDecl_Iterator_for_Once<T>
                      AdtDecl_Once<T>
                      TraitImplDecl_Copy_for_Once<T>
                      ])))

    (CrateDecls
     (term [core-crate-decl
            CrateDecl_C
            ]))

    ]

   (; Cannot normalize `(T,)` to `T`
    traced '()
           (test-equal
            (term (decl:can-prove-goal CrateDecls C ((user-ty (< (tuple u32) as Iterator[] > :: Item[]))
                                                     ==
                                                     (user-ty u32))))
            #f))

   (; Can normalize `(T,)` to `Once<T>`
    traced '()
           (test-equal
            (term (decl:can-prove-goal CrateDecls C ((user-ty (< (tuple u32) as Iterator[] > :: Item[]))
                                                     ==
                                                     (user-ty (Once < u32 >)))))
            #t))

   (; Test that `<(u32,) as Iterator>::Item` implements `Copy`
    traced '()
           (test-equal
            (term (decl:can-prove-goal
                   CrateDecls C
                   (is-implemented (core:Copy[(user-ty (< (tuple u32) as Iterator[] > :: Item[]))]))))
            #t))
   )
  )