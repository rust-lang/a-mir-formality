#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         "libcore.rkt"
         )

(module+ test
  (redex-let*
   formality-decl

   [(; trait Get { fn get<T>(&mut self) where T: Debug; }
     TraitDecl_Get
     (term (trait Get ((type Self)) where ()
                  {
                   (fn get[(type T) (lifetime l)]((user-ty (&mut l Self))) -> (user-ty ()) where [(T : Debug[])] {})
                   }))
     )

    (; impl Get for () { fn get<T>(&mut self) /* where T: Debug */ ;
     ;                                           -------------- OK}
     TraitImplDecl_Get
     (term (impl[] (Get[(user-ty ())]) where []
                {(fn get[(type T) (lifetime l)]((user-ty (&mut l ()))) -> (user-ty ()) where [] {})
                 }))
     )
    ]

   (traced '()
           (test-equal
            (term (decl:is-crate-ok [core-crate-decl (crate C {TraitDecl_Get TraitImplDecl_Get})] C))
            #t))
   )
  )
