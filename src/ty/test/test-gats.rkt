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
                  ∀ ((type T)) (is-implemented (Iterable ((user-ty (Vec T))))))
                 (; normalizes-to `Item<Vec<T>, A>` to `(rigid-ty (ref ()) (A T))`
                  ∀ ((type T) (lifetime A)) (normalizes-to (user-ty (@ Item (Vec T) A))
                                                           (user-ty (& A T))))
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
                   ((∀ ((type T) (lifetime A))) (∃ ((type U))))
                   ()
                   (normalizes-to (user-ty (@ Item (Vec T) A)) (user-ty (& A T)))
                   )
                  )
            ))
   )
  )