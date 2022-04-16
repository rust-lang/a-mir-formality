#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../scheme.rkt"
         "../../util.rkt"
         "../../logic/instantiate.rkt"
         )

(module+ test

  (define-metafunction formality-ty
    ;; convenience for testing: write `(item T)` to reference the alias type `Item`
    item : Ty -> AliasTy

    [(item Ty) (TyAlias Item (Ty))]
    )

  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore WellFormed rules, not interesting for testing subtyping
                  ForAll ((TyKind T)) (WellFormed (TyKind T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ForAll ((TyKind T)) (Implemented (AlwaysImpl (T))))
                 (; Normalize `Item<Vec<T>>` to `T`
                  ForAll ((TyKind T)) (Normalize (item (vec T)) T))
                 )
                ()
                ()
                )))
    )




   (traced '()
           (; #25860 -- an upcast that discharges implied bound successfully
            test-match
            formality-ty
            ((Exists () (Implies () _))) ; provable!
            (term (ty:prove-scheme
                   Env
                   ()
                   ()
                   ((item (vec (scalar-ty i32)))
                    <=
                    (scalar-ty i32)
                    )
                   )
                  )
            ))
   )
  )
