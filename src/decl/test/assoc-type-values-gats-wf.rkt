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
    trait-decl-of-LendingIterator : WhereClauses -> TraitDecl

    [(trait-decl-of-LendingIterator WhereClauses)
     (trait LendingIterator[(type Self)] where ()
            {
             (type Item ((lifetime a)) (: (type Self) [])
                   where WhereClauses)
             })
     ]
    )

  (redex-let*
   formality-decl

   [(; trait LendingIterator { type Item<'a> where Self: 'a; }
     TraitDecl_LendingIterator_with_Item_where_Self:a
     (term (trait-decl-of-LendingIterator [(Self : a)])))

    (; trait LendingIterator { type Item<'a>; }
     TraitDecl_LendingIterator_without_Item_where_Self:a
     (term (trait-decl-of-LendingIterator [])))

    (; impl<T> Iterator for Lend<T> { type Item<'a> = &'a T where T : 'a; }
     TraitImplDecl_LendingIterator_for_Lend<T>
     (term (impl((type T)) (LendingIterator ((user-ty (Lend T))))
                where ()
                {
                 (type Item ((lifetime a)) = (user-ty (& a T))
                       where [(T : a)])
                 })))

    (; struct Lend<T> { }
     AdtDecl_Lend<T>
     (term (struct Lend ((type T))
             where ()
             ((Lend ())))))

    (; Crate C has LendingIterator trait WITH the where clause
     CrateDecl_C
     (term (C (crate [TraitDecl_LendingIterator_with_Item_where_Self:a
                      TraitImplDecl_LendingIterator_for_Lend<T>
                      AdtDecl_Lend<T>
                      ]))))

    (; Crate D has LendingIterator trait WITHOUT the where clause
     CrateDecl_D
     (term (D (crate [TraitDecl_LendingIterator_without_Item_where_Self:a
                      TraitImplDecl_LendingIterator_for_Lend<T>
                      AdtDecl_Lend<T>
                      ]))))

    ]

   (
    traced '()
           (test-equal (term (decl:is-crate-ok [core-crate-decl CrateDecl_C] C))
                       #f ; FIXME(#63) -- we don't expand outlives bounds
                       ))
   )
  )