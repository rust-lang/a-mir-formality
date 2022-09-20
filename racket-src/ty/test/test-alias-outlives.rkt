#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../../util.rkt"
         "../user-ty.rkt"
         )

(module+ test

  (define-metafunction formality-ty
    ;; convenience for testing: write `(item T)` to reference the alias type `Item`
    item : UserTy -> AliasTy

    [(item UserTy) (alias-ty Item ((user-ty UserTy)))]
    )

  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore well-formed rules, not interesting for testing subtyping
                  ∀ ((type T)) (well-formed (type T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((type T)) (is-implemented (AlwaysImpl (T))))
                 (; normalizes-to `Item<Vec<T>>` to `T`
                  ∀ ((type T)) (normalizes-to (item (Vec < T >)) T))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ()
                   ()
                   ((item i32)
                    -outlives-
                    static
                    )
                   )
                  )
            (term ty:provable)
            ))
   )
  )
