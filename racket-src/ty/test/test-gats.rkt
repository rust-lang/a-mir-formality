#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../scheme.rkt"
         "../user-ty.rkt"
         "../../util.rkt"
         "../../logic/instantiate.rkt"
         )

(module+ test

  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore well-formed rules, not interesting for testing subtyping
                  ∀ ((type T)) (well-formed (type T)))
                 (; Define a trait `Iterable` that is implemented for Vec<T>
                  ∀ ((type T)) (is-implemented (Iterable ((user-ty (Vec < T >))))))
                 (; normalizes-to `Item<Vec<T>, A>` to `(rigid-ty (ref ()) (A T))`
                  ∀ ((type T) (lifetime a)) (normalizes-to (user-ty (< (Vec < T >) as Iterator[] > :: Item[a]))
                                                           (user-ty (& a T))))
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
                   ((∀ ((type E) (lifetime l))) (∃ ((type U))))
                   ()
                   (normalizes-to (user-ty (< (Vec < E >) as Iterator[] > :: Item[l]))
                                  (user-ty (& l E)))
                   )
                  )
            ))
   )
  )