#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

#;(module+ test
    (redex-let*
     formality-decl

     [(; trait Wf { }
       TraitDecl_Wf
       (term (trait Wf ((type Self)) where [] {})))

      (; impl<T> Wf for T { }
       TraitImplDecl_Wf_for_T
       (term (impl[(type T)] (Wf[T]) where [] {})))

      (; struct RequiresOrd<'a, T: Ord>
       AdtDecl_RequiresOrd
       (term (struct RequiresOrd[(lifetime a) (type T)]
               where [(T : core:Ord[])]
               {})))

      (CrateDecl (C))
      ]

     (traced '()
             (test-equal
              (decl:is-crate-ok
               [core-crate-decl ]

               )
              #t))
     )
    )
