#lang racket
(require redex
         "../grammar.rkt"
         "../substitution.rkt"
         "../instantiate.rkt"
         "../hook.rkt"
         "../unify.rkt")

(provide reset
         equate)

(define-metafunction formality-ty
  ;; Returns a version of `Env_old` where the universes/substitution of any
  ;; variables may be adjused based on `Env_new` but it is otherwise unchanged.
  ;;
  ;; Currently requires that (a) new variables are introduced as a prefix
  ;; onto the environment and (b) only the `VarIds_new` have been introduced.
  ;; Could be written to be much more accepting but this version is meant to catch
  ;; bugs elsewhere, since we expect (a) and (b) to hold.
  reset : Env_old VarIds_new Env_new -> Env

  [(reset
    (Hook Universe ((VarId_old Quantifier_old Universe_old) ...) Substitution_0 Hypotheses) ; Env_old
    (VarId_new ...) ; VarIds_new
    (Hook _ VarBinders_new Substitution_new _) ; Env_new
    )
   (Hook Universe ((VarId_old Quantifier_old Universe_new) ...) Substitution_out Hypotheses)

   (where/error ((VarId_new _ _) ... (VarId_old Quantifier_old Universe_new) ...) VarBinders_new)
   (where/error Substitution_out (substitution-without-vars Substitution_new (VarId_new ...)))
   ]
  )


(define-judgment-form formality-ty
  #:mode (equate I I I O)
  #:contract (equate Env Term Term Env_out)

  [(where Env_out (most-general-unifier Env ((Term_1 Term_2))))
   ----------------
   (equate Env Term_1 Term_2 Env_out)]

  )


(module+ test
  (redex-let*
   formality-ty
   ((; A is in U0
     (Env_0 Ty_A (VarId_0)) (term (instantiate-quantified EmptyEnv (Exists ((TyKind A)) A))))
    (; V is a placeholder in U1
     (Env_1 Ty_T (VarId_1)) (term (instantiate-quantified Env_0 (ForAll ((TyKind T)) T))))
    (; X is in U1
     (Env_2 Ty_X (VarId_2)) (term (instantiate-quantified Env_1 (Exists ((TyKind X)) X))))
    (; Y, Z are in U1
     (Env_3 (Ty_Y Ty_Z) VarIds_3) (term (instantiate-quantified Env_2 (Exists ((TyKind Y) (TyKind Z)) (Y Z)))))
    (Env_4 (term (most-general-unifier Env_3 (((TyRigid Vec (Ty_A)) (TyRigid Vec (Ty_X)))))))
    (Env_5 (term (reset Env_2 VarIds_3 Env_4)))
    )

   (test-equal (term (reset Env_0 (VarId_1) Env_1)) (term Env_0))
   (test-equal (term (reset Env_0 (VarId_2 VarId_1) Env_2)) (term Env_0))
   )
  )