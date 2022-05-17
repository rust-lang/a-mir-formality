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
                  ∀ ((TyKind T)) (WellFormed (TyKind T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((TyKind T)) (Implemented (AlwaysImpl (T))))
                 )
                ()
                ()
                )))
    )

   (traced '()

           ; Given a T:
           ;
           ; &'a T -outlives- &'b T if
           ;
           ; 'a -outlives- 'b

           (test-match
            formality-ty

            ((Exists
              ((LtKind VarId_B) (LtKind VarId_A))
              (Implies
               ((VarId_A -outlives- VarId_B))
               ((TyRigid (Ref ()) (VarId_A VarId_T))
                -outlives-
                (TyRigid (Ref ()) (VarId_B VarId_T))))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((TyKind T)))
                    (Exists ((LtKind A) (LtKind B))))
                   ()
                   ((TyRigid (Ref ()) (A T)) -outlives- (TyRigid (Ref ()) (B T)))
                   ))
            )
           )

   (traced '()
           (test-match
            formality-ty

            ; for<'a> fn(&'a T) -outlives- static

            ((Exists
              ()
              (Implies
               ()
               ((∀
                 ((LtKind A))
                 (TyRigid
                  (Fn "" 1)
                  ((TyRigid (Ref ()) (A (TyRigid u32 ())))
                   (TyRigid (Tuple 0) ()))))
                -outlives-
                static))))

            (term (ty:prove-scheme
                   Env
                   ()
                   ()
                   ((∀ ((LtKind A)) (TyRigid (Fn "" 1) ((TyRigid (Ref ()) (A (scalar-ty u32))) TyUnit)))
                    -outlives-
                    static)
                   ))
            )
           )

   (traced '()
           (test-match
            formality-ty

            ; For some T, exists A where (A : T)...
            ;
            ; true if (A -outlives- 'static)

            ((Exists
              ((LtKind VarId_A))
              (Implies
               ((VarId_A -outlives- static))
               (VarId_A -outlives- VarId_T))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((TyKind T)))
                    (Exists ((LtKind A))))
                   ()
                   (A
                    -outlives-
                    T)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B, cannot prove
           ;
           ; !A -outlives- !B

           (test-match
            formality-ty

            ()

            (term (ty:prove-scheme
                   Env
                   ((∀ ((LtKind A) (LtKind B))))
                   ()
                   (A -outlives- B)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B where !A:!B, can prove
           ;
           ; !A -outlives- !B

           (test-match
            formality-ty

            ((Exists () (Implies () (VarId_A -outlives- VarId_B))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((LtKind A) (LtKind B))))
                   ((Outlives (A : B)))
                   (A -outlives- B)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B where !A:!B, cannot prove A == B

           (test-match
            formality-ty

            ()

            (term (ty:prove-scheme
                   Env
                   ((∀ ((LtKind A) (LtKind B))))
                   ((Outlives (A : B))
                    )
                   (A == B)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B where !A:!B and !B:!A, can prove A == B

           (test-match
            formality-ty

            ((Exists () (Implies () (VarId_A == VarId_B))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((LtKind A) (LtKind B))))
                   ((Outlives (A : B))
                    (Outlives (B : A))
                    )
                   (A == B)
                   ))
            )
           )
   )
  )