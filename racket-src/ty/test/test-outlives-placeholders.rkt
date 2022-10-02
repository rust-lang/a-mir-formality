#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../user-ty.rkt"
         "../../util.rkt"
         )

(module+ test
  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore well-formed rules, not interesting for testing subtyping
                  ∀ ((type T)) (well-formed (type T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((type T)) (is-implemented (AlwaysImpl (T))))
                 )
                ()
                ()
                )))
    )

   ; Given `E1 : P1`, we add an edge `E1 -outlives- P1`.
   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime P1)))
                    (∃ ((lifetime E1))))
                   ()
                   (E1 -outlives- P1)
                   ))
            (term [(:- () (() ((E1 -outlives- P1))))])))

   ; Given `P1 : E1`, we add an edge `E1 -outlived-by- P1`
   ; (because all edges originate in inference variables).
   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime P1)))
                    (∃ ((lifetime E1))))
                   ()
                   (P1 -outlives- E1)
                   ))
            (term [(:- () (() ((E1 -outlived-by- P1))))])))

   ; We cannot have `P1 : E1` and `E1 : P2` because we don't
   ; know that `P1 : P2`.
   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime P1) (lifetime P2)))
                    (∃ ((lifetime E1))))
                   ()
                   (&& [(P1 -outlives- E1)
                        (E1 -outlives- P2)
                        ])
                   ))
            (term [])))

   ; We can have `P1 : E1` and `E1 : P2` because we
   ; know that `P1 : P2`.
   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime P1) (lifetime P2)))
                    (∃ ((lifetime E1))))
                   ((P1 -outlives- P2))
                   (&& [(P1 -outlives- E1)
                        (E1 -outlives- P2)
                        ])
                   ))
            (term [(:- () (() ((E1 -outlives- P2)
                               (E1 -outlived-by- P1))))
                   ])))
   )
  )
