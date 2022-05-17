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
                ((; Just ignore well-formed rules, not interesting for testing subtyping
                  ∀ ((type T)) (well-formed (type T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((type T)) (Implemented (AlwaysImpl (T))))
                 (; normalizes-to `Item<Vec<T>>` to `T`
                  ∀ ((type T)) (normalizes-to (item (vec T)) T))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (test-match
            formality-ty
            ((∃ () (implies () _))) ; provable!
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
