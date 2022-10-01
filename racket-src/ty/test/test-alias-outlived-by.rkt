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
                 (; normalizes-to `Item<Vec<T>>` to `T`
                  ∀ ((type T)) (normalizes-to (item (Vec < T >)) T))
                 )
                ()
                ()
                )))
    )

   ; We cannot normalize `Item<&'l1 i32>`, so the only way to prove
   ; that `∃l1 : Item<&'l1 i32>` is if `l1: 'static`.
   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ((∃ [(lifetime l1)]))
                   ()
                   (l1 -outlives- (item (& l1 i32)))
                   )
                  )
            (term [(:- () (() ((l1 -outlives- static))))])
            ))

   ; We can normalize `Item<Vec<&'l1 i32>>` to `&'l1 i32`, so
   ; we can prove that `∃l1 : Item<&'l1 i32>` is just true for any `l1`.
   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ((∃ [(lifetime l1)]))
                   ()
                   (l1 -outlives- (item (Vec < (& l1 i32) >)))
                   )
                  )
            (term ty:provable)
            ))

   )
  )
