#lang racket
(require redex/reduction-semantics
         racket/set
         "grammar.rkt"
         "env.rkt"
         "instantiate.rkt"
         "substitution.rkt"
         )
(provide querify-goal
         )

(define-metafunction formality-logic
  ;; *Querifying* a term `Term` from an env `Env`
  ;; returns a `Query` that is independent from the
  ;; environment.
  ;;
  ;; Any free variable in `Term` that was bound
  ;; in the environment is instead bound in the `Query`.
  ;; The universes from `Query` are remapped to sequential
  ;; universes that preserve the relative unvierse relationships.
  ;;
  ;; The `UniversePairs` mapping that is returned can be used to map
  ;; back from the universes in `Query` to the original universes
  ;; in `Env`.
  querify-goal : Env Goal -> (QueryGoal UniversePairs)
  [(querify-goal Env Goal_0)
   ((?- (VarBinder_map ...) (implies Hypotheses_e Goal_e))
    (reverse-universe-map UniversePairs_map))

   (where/error Goal_e (apply-substitution-from-env Env Goal_0))
   (where/error Hypotheses_e (apply-substitution-from-env Env (env-hypotheses Env)))

   ; find the free variables in Term and the binding info about them from env
   (where/error (VarId_free ...) (free-variables Env (Goal_e Hypotheses_e)))
   (where/error (VarBinder_free ...) ((var-binding-in-env Env VarId_free) ...))

   ; find the universes that these free variables refer to
   (where/error ((_ _ _ Universe_free) ...) (VarBinder_free ...))
   (where/error (Universe_all ...) (RootUniverse Universe_free ...))

   ; compress the universes in Term
   (where/error UniversePairs_map (universe-map (Universe_all ...)))
   (where/error (VarBinder_map ...) (apply-universe-map UniversePairs_map (VarBinder_free ...)))
   ]
  )

(define-metafunction formality-logic
  ;; Replace all universes in `Term` with their replacements from `UniversePairs`.
  ;;
  ;; Precondition: All universes appearing in `Term` must also be mapped by `UniversePairs`.
  apply-universe-map : UniversePairs Term -> Term

  [(apply-universe-map UniversePairs Universe)
   Universe_1
   (where/error (_ ... (Universe Universe_1) _ ...) UniversePairs)]

  [(apply-universe-map UniversePairs (Term ...))
   ((apply-universe-map UniversePairs Term) ...)]

  [(apply-universe-map UniversePairs Term)
   Term]
  )

(define-metafunction formality-logic
  ;; Convert a map `U0 -> U1` to `U1 -> U0`.
  reverse-universe-map : UniversePairs -> UniversePairs

  [(reverse-universe-map ((Universe_from Universe_to) ...))
   ((Universe_to Universe_from) ...)]
  )

(define-metafunction formality-logic
  ;; Given a set of universes extracted from some term that is being querifyd,
  ;; returns a "universe map" that maps from each distinct universe to a new, more company
  ;; universe to be used in the canonical form. The new universes preserve the relative ordering
  ;; from the input.
  ;;
  ;; Example: given `(U0 U1 U1 U5)`, would return `((U0 U0) (U1 U1) (U5 U2))`; note that each
  ;; distinct universe from the input (`U0`, `U1`, `U5`) is mapped to a (consecutive) universe
  ;; such that their relative orderings are preserved.
  universe-map : Universes -> (UniversePair ...)

  ((universe-map Universes)
   (number-universes 0 Universes_sorted)
   (where/error Universes_dedup (union-sets (RootUniverse) Universes))
   (where/error Universes_sorted ,(sort (term Universes_dedup) (lambda (a b) (term (universe-includes ,b ,a)))))
   )
  )

(define-metafunction formality-logic
  number-universes : number Universes -> (UniversePair ...)

  [(number-universes number ()) ()]

  [(number-universes number (Universe_0 Universe_1 ...))
   ((Universe_0 (universe number)) UniversePair ...)
   (where/error number_1 ,(+ (term number) 1))
   (where/error (UniversePair ...) (number-universes number_1 (Universe_1 ...)))
   ]
  )

(module+ test
  (require "test/hook.rkt"
           "../util.rkt"
           )
  (test-equal (term (universe-map ((universe 1) (universe 1) (universe 3) (universe 7))))
              (term (((universe 0) (universe 0))
                     ((universe 1) (universe 1))
                     ((universe 3) (universe 2))
                     ((universe 7) (universe 3)))))

  (test-equal (term (apply-universe-map (((universe 0) (universe 1)) ((universe 1) (universe 0))) (a b (universe 0) (universe 1))))
              (term (a b (universe 1) (universe 0))))

  (redex-let*
   formality-logic

   (((Env_0 () (Parameter_A Parameter_B)) (term (instantiate-quantified EmptyEnv (∀ ((type A) (type B)) ()))))
    ((Env_1 () (Parameter_X)) (term (instantiate-quantified Env_0 (∀ ((type X)) ()))))
    ((Env_2 () (Parameter_Z)) (term (instantiate-quantified Env_1 (∀ ((type Z)) ()))))
    )

   (traced '()
           (test-equal
            (term (querify-goal Env_2 (is-true (Parameter_A Parameter_Z))))
            (term ((?-
                    ((Parameter_A type ∀ (universe 1))
                     (Parameter_Z type ∀ (universe 2))
                     )
                    (implies ()
                             (is-true (Parameter_A Parameter_Z))))
                   (((universe 0) (universe 0))
                    ((universe 1) (universe 1))
                    ((universe 2) (universe 3)))))))
   )

  (redex-let*
   formality-logic

   (((Env_0 () (Parameter_A Parameter_B)) (term (instantiate-quantified EmptyEnv (∀ ((type A) (type B)) ()))))
    ((Env_1 () (Parameter_X)) (term (instantiate-quantified Env_0 (∀ ((type X)) ()))))
    ((Env_2 () (Parameter_Z)) (term (instantiate-quantified Env_1 (∀ ((type Z)) ()))))
    (Env_3 (term (env-with-hypotheses Env_2 [(is-true Parameter_X)])))
    )

   (traced '()
           (test-equal
            (term (querify-goal Env_3 (is-true Parameter_A)))
            (term ((?-
                    ((Parameter_X type ∀ (universe 2))
                     (Parameter_A type ∀ (universe 1))
                     )
                    (implies [(is-true Parameter_X)]
                             (is-true Parameter_A)))
                   (((universe 0) (universe 0))
                    ((universe 1) (universe 1))
                    ((universe 2) (universe 2)))))))
   )
  )