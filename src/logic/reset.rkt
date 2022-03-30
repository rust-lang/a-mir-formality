#lang racket
(require redex
         "grammar.rkt"
         "substitution.rkt"
         "inequalities.rkt"
         "instantiate.rkt")

(provide reset)

(define-metafunction formality-logic
  ;; Returns a version of `Env_old` where the universes/substitution of any
  ;; variables may be adjused based on `Env_new` but it is otherwise unchanged.
  ;;
  ;; Currently requires that (a) new variables are introduced as a prefix
  ;; onto the environment and (b) only the `VarIds_new` have been introduced.
  ;; Could be written to be much more accepting but this version is meant to catch
  ;; bugs elsewhere, since we expect (a) and (b) to hold.
  reset : Env_old VarIds_new Env_new -> Env

  [(reset
    (Hook Universe ((VarId_old Quantifier_old Universe_old) ...) _ _ Hypotheses) ; Env_old
    (VarId_new ...) ; VarIds_new
    (Hook _ VarBinders_new Substitution_new VarInequalities_new _) ; Env_new
    )
   (Hook Universe ((VarId_old Quantifier_old Universe_new) ...) Substitution_out VarInequalities_out Hypotheses)

   (where/error ((VarId_new _ _) ... (VarId_old Quantifier_old Universe_new) ...) VarBinders_new)
   (where/error Substitution_out (substitution-without-vars Substitution_new (VarId_new ...)))
   (where/error VarInequalities_out (inequalities-without-vars VarInequalities_new (VarId_new ...)))
   ]
  )

(module+ test
  (require "test/hook.rkt")

  (redex-let*
   formality-logic
   ((; A is in U0
     (Env_0 Term_A (VarId_0)) (term (instantiate-quantified EmptyEnv (Exists ((TyKind A)) A))))
    (; V is a placeholder in U1
     (Env_1 Term_T (VarId_1)) (term (instantiate-quantified Env_0 (ForAll ((TyKind T)) T))))
    (; X is in U1
     (Env_2 Term_X (VarId_2)) (term (instantiate-quantified Env_1 (Exists ((TyKind X)) X))))
    )

   (test-equal (term (reset Env_0 (VarId_1) Env_1)) (term Env_0))
   (test-equal (term (reset Env_0 (VarId_2 VarId_1) Env_2)) (term Env_0))
   )
  )