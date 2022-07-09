#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(Rust/TraitDecl_gat (term (trait Ref[]
                                     where []
                                     { (type Item[(lifetime a)] : [] where [(Self : a)])
                                       })))

    (Rust/TraitImplDecl_gat (term (impl[(type T)] Ref[] for T
                                       where []
                                       { (type Item[(lifetime a)] = (& a T) where [(T : a)])
                                         })))

    (Rust/Program (term ([(crate C { Rust/TraitDecl_gat
                                     Rust/TraitImplDecl_gat
                                     })] C)))

    ]

   (traced '()
           (test-term-true
            (rust:can-prove-where-clause-in-program
             Rust/Program
             (∀ [(lifetime a)]
                where []
                ; key point here:
                ;
                ;     this is actually only valid for `l` where `a: l`, but the default
                ;     bounds make it provable.
                (for[(lifetime l)] (< (& a ()) as Ref[] > :: Item[l] == (& l (& a ()))))
                )
             )
            ))
   )
  )
