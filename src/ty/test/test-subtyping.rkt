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
           (ty:test-can-prove Env ((ForAll ((TyKind T)) T) <= (scalar-ty u32)))
           )

   (traced '()
           (ty:test-can-prove Env ((scalar-ty u32) >= (ForAll ((TyKind T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((scalar-ty u32) <= (ForAll ((TyKind T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((ForAll ((TyKind T)) T) >= (scalar-ty u32)))
           )

   ; Cannot have an implication on the subtype side that doesn't appear on the supertype side
   (traced '()
           (ty:test-cannot-prove Env ((ForAll ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))
                                      <=
                                      (ForAll ((TyKind T)) T)))
           )

   ; ...unless we can prove it.
   (traced '()
           (ty:test-can-prove Env ((ForAll ((TyKind T)) (Implies ((Implemented (AlwaysImpl (T)))) T))
                                   <=
                                   (ForAll ((TyKind T)) T)))
           )

   ; OK if the implication is on both sides.
   (traced '()
           (ty:test-can-prove Env ((ForAll ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))
                                   <=
                                   (ForAll ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))))
           )

   ; OK if the implication is just on supertype side: that means that the consumer will prove it,
   ; but in this case it's not really needed anyway.
   (traced '()
           (ty:test-can-prove Env ((ForAll ((TyKind T)) T)
                                   <=
                                   (ForAll ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))))
           )

   (redex-let*
    formality-ty
    [((Env_1 () (Ty_T)) (term (instantiate-quantified Env (ForAll ((TyKind T)) ()))))
     ((Env_2 () (Ty_U)) (term (instantiate-quantified Env_1 (Exists ((TyKind U)) ()))))
     (Goal (term (Ty_T <= Ty_U)))
     ((Env_out ...) (judgment-holds (ty:prove-top-level-goal/cosld Env_2 Goal Env_out) Env_out))
     ((Scheme_out ...) (term ((extract-scheme Env_out Goal) ...)))
     ]
    (test-equal
     (term (Scheme_out ...))
     (term ((Exists ((TyKind Ty_U)) (Implies ((Ty_U >= Ty_T)) (Ty_T <= Ty_U))))))
    )

   (redex-let*
    formality-ty
    [((Env_1 () (Ty_T)) (term (instantiate-quantified Env (ForAll ((TyKind T)) ()))))
     ((Env_2 () (Ty_U Lt_A)) (term (instantiate-quantified Env_1 (Exists ((TyKind U) (LtKind A)) ()))))
     (Goal (term (Ty_U <= (TyRigid (Ref ()) (Lt_A Ty_T)))))
     ((Env_out ...) (judgment-holds (ty:prove-top-level-goal/cosld Env_2 Goal Env_out) Env_out))
     ((Scheme_out ...) (term ((extract-scheme Env_out Goal) ...)))
     ]
    (test-match
     formality-ty
     ((Exists
       ((LtKind VarId_A)
        (TyKind VarId_TheTy)
        (LtKind VarId_TheLt))
       (Implies
        ((VarId_TheTy <= VarId_T)
         (VarId_TheLt <= VarId_A))
        ((TyRigid (Ref ()) (VarId_TheLt VarId_TheTy))
         <=
         (TyRigid (Ref ()) (VarId_A VarId_T))))))
     (term (Scheme_out ...)))
    )

   )
  )