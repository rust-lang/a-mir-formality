#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../scheme.rkt"
         "../../util.rkt"
         "../../logic/instantiate.rkt"
         )

(module+ test
  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore WellFormed rules, not interesting for testing subtyping
                  ForAll ((TyKind T)) (WellFormed (TyKind T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ForAll ((TyKind T)) (Implemented (AlwaysImpl (T))))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (test-match
            formality-ty

            ((Exists
              ((LtKind B«4»1) (LtKind A«3»1))
              (Implies
               ((A«3»1 -outlives- B«4»1))
               ((TyRigid (Ref ()) (A«3»1 T«0»1))
                -outlives-
                (TyRigid (Ref ()) (B«4»1 T«0»1))))))

            (term (ty:prove-scheme
                   Env
                   ((ForAll ((TyKind T)))
                    (Exists ((LtKind A) (LtKind B))))
                   ((TyRigid (Ref ()) (A T)) -outlives- (TyRigid (Ref ()) (B T)))
                   ))
            )
           )
   )
  )