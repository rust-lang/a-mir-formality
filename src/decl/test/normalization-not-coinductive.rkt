#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         )

(module+ test
  (redex-let*
   formality-decl

   ((Ty_Unit (term (user-ty ())))
    (; trait Foo { type FooItem; }
     TraitDecl_Foo (term (trait Foo[(type Self)] where [] {
                                                           (type FooItem[] (: (type T) []) where [])
                                                           })))
    (; trait Bar { type BarItem; }
     TraitDecl_Bar (term (trait Bar[(type Self)] where [] {
                                                           (type BarItem[] (: (type T) []) where [])
                                                           })))
    (; impl Foo for () {
     ;     type FooItem = <() as Bar>::BarItem;
     ; }
     TraitImplDecl_FooImpl (term (impl[] (Foo[Ty_Unit]) where [] {
                                                                  (type FooItem[] = (user-ty (< () as Bar[] > :: BarItem[])) where [])
                                                                  })))
    (; impl Bar for () {
     ;     type BarItem = <() as Foo>::FooItem;
     ; }
     TraitImplDecl_BarImpl (term (impl[] (Bar[Ty_Unit]) where [] {
                                                                  (type BarItem[] = (user-ty (< () as Foo[] > :: FooItem[])) where [])
                                                                  })))
    (CrateDecl (term (TheCrate (crate (TraitDecl_Foo TraitDecl_Bar TraitImplDecl_FooImpl TraitImplDecl_BarImpl)))))
    )

   (traced '()
           (test-equal
            (term (decl:can-prove-goal
                   [CrateDecl]
                   TheCrate
                   (∀ ((type T))
                      (normalizes-to (alias-ty (Foo FooItem) [Ty_Unit]) T))))
            #f)

           )
   )
  )