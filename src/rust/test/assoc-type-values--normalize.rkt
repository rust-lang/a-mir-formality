#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (current-traced-metafunctions '(relate/one compare/one/substituted equate/one/substituted))

  (redex-let*
   formality-rust

   [(; trait Iterator { type Item; }
     Rust/TraitDecl_Iterator_where_Item
     (term (trait Iterator[] where ()
                  {
                   (type Item () : [] where [])
                   }))
     )

    (; impl<T> Iterator for (T,) { type Item = Once; }
     Rust/TraitImplDecl_Iterator_for_Once<T>
     (term (impl[(type T)] Iterator[] for (tuple T)
                where []
                {
                 (type Item[] = (Once < T >) where [])
                 })))

    (; struct Once<T> { }
     Rust/AdtDecl_Once<T>
     (term (struct Once ((type T))
             where ()
             {})))

    (; impl<T> Copy for Once<T> where T: Copy { }
     Rust/TraitImplDecl_Copy_for_Once<T>
     (term (impl[(type T)] core:Copy[] for (Once < T >)
                where [(T : core:Copy[])]
                {})))

    (Rust/CrateDecl_C
     (term (crate C [ Rust/TraitDecl_Iterator_where_Item
                      Rust/TraitImplDecl_Iterator_for_Once<T>
                      Rust/AdtDecl_Once<T>
                      Rust/TraitImplDecl_Copy_for_Once<T>
                      ])))

    (Rust/Program
     (term ([libcore Rust/CrateDecl_C] C)))
    ]

   (; Cannot normalize `(T,)` to `T`
    traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program
                   Rust/Program
                   (< (tuple u32) as Iterator[] > :: Item[] == u32)))
            #f))

   (; Can normalize `(T,)` to `Once<T>`
    traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program
                   Rust/Program
                   (< (tuple u32) as Iterator[] > :: Item[] == (Once < u32 >))))
            #t))

   (; Test that `<(u32,) as Iterator>::Item` implements `Copy`
    traced '()
           (test-equal
            (term (rust:can-prove-where-clause-in-program
                   Rust/Program
                   ((< (tuple u32) as Iterator[] > :: Item[]) : core:Copy[])))
            #t))
   )
  )