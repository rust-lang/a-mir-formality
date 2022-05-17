#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../../util.rkt"
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
                  ∀ ((TyKind T)) (WellFormed (TyKind T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((TyKind T)) (Implemented (AlwaysImpl (T))))
                 (; Normalize `Item<Vec<T>>` to `T`
                  ∀ ((TyKind T)) (Normalize (item (vec T)) T))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (test-match
            formality-ty
            ((Exists () (Implies () _))) ; provable!
            (term (ty:prove-scheme
                   Env
                   ()
                   ()
                   ((item (scalar-ty i32))
                    -outlives-
                    static
                    )
                   )
                  )
            ))
   )
  )
