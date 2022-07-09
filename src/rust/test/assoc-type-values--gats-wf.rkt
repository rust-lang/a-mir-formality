#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (current-traced-metafunctions '(relate/one compare/one/substituted equate/one/substituted))

  (define-metafunction formality-rust
    trait-decl-of-LendingIterator : Rust/WhereClauses -> Rust/TraitDecl

    [(trait-decl-of-LendingIterator Rust/WhereClauses)
     (trait LendingIterator[] where [] { Rust/AssociatedTyDecl })
     (where/error Rust/AssociatedTyDecl (type Item[(lifetime a)] : [] where Rust/WhereClauses))
     ]
    )

  (redex-let*
   formality-rust

   [(; trait LendingIterator { type Item<'a> where Self: 'a; }
     Rust/TraitDecl_LendingIterator_with_Item_where_Self:a
     (term (trait-decl-of-LendingIterator [(Self : a)])))

    (; trait LendingIterator { type Item<'a>; }
     Rust/TraitDecl_LendingIterator_without_Item_where_Self:a
     (term (trait-decl-of-LendingIterator [])))

    (; impl<T> Iterator for Lend<T> { type Item<'a> = &'a T where T : 'a; }
     Rust/TraitImplDecl_LendingIterator_for_Lend<T>
     (term (impl[(type T)] LendingIterator[] for (Lend < T >)
                where []
                {
                 (type Item[(lifetime a)] = (& a T)
                       where [(T : a)])
                 })))

    (; struct Lend<T> { }
     Rust/AdtDecl_Lend<T>
     (term (struct Lend[(type T)]
             where []
             {})))

    (; Crate C has LendingIterator trait WITH the where clause
     Rust/CrateDecl_C
     (term (crate C { Rust/TraitDecl_LendingIterator_with_Item_where_Self:a
                      Rust/TraitImplDecl_LendingIterator_for_Lend<T>
                      Rust/AdtDecl_Lend<T>
                      })))

    (; Crate D has LendingIterator trait WITHOUT the where clause
     Rust/CrateDecl_D
     (term (crate D { Rust/TraitDecl_LendingIterator_without_Item_where_Self:a
                      Rust/TraitImplDecl_LendingIterator_for_Lend<T>
                      Rust/AdtDecl_Lend<T>
                      })))

    ]

   (traced '()
           (test-equal (term (rust:is-program-ok ([libcore Rust/CrateDecl_C] C)))
                       #t
                       ))

   (traced '()
           (test-equal (term (rust:is-program-ok ([libcore Rust/CrateDecl_D] D)))
                       #f
                       ))
   )
  )