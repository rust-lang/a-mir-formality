#lang racket

(require redex/reduction-semantics
         "../grammar.rkt"
         "../../logic/cosld-solve.rkt"
         "../../logic/env.rkt"
         "../../logic/instantiate.rkt"
         "../hook.rkt"
         "../relate.rkt"
         "../scheme.rkt"
         "../predicate.rkt"
         "../where-clauses-from-env.rkt"
         )
(provide env-with-clauses-invariants-and-generics
         EmptyEnv
         ty:prove-top-level-goal/cosld
         ty:test-can-prove
         ty:test-cannot-prove
         ty:prove-scheme
         )

(define-metafunction formality-ty
  ;; Creates a suitable env for testing the ty-logic layer
  ;; that has the given clauses, invariants, and adt-declarations.
  env-with-clauses-invariants-and-generics : Clauses Invariants ((AdtId Generics) ...) -> Env

  [(env-with-clauses-invariants-and-generics Clauses Invariants ((AdtId Generics) ...))
   (env-with-hook (Hook: ,(begin
                            (formality-ty-hook
                             (lambda (env predicate) (term Clauses))
                             (lambda () (term Invariants))
                             (lambda (env relation) (term (ty:relate-parameters ,env ,relation)))
                             (lambda (env predicate) (term Error)) ; no built-in predicates
                             (lambda (goal) (term (ty:is-predicate? ,goal)))
                             (lambda (predicate) (term #f)) ; no built-in predicates
                             (lambda (predicate1) (term (ty:debone-predicate ,predicate1)))
                             (lambda (goal) (term (ty:is-relation? ,goal)))
                             (lambda (adt-id) (term (find-adt-generics ,adt-id ((AdtId Generics) ...))))
                             (lambda (where-clause) (term (where-clause->goal∧clause-mock ,where-clause)))
                             ))))
   ]
  )

;; A suitable env for testing the formality-ty layer in isolation
;; with no clauses, invariants, nor generics.
(define-term EmptyEnv (env-with-clauses-invariants-and-generics () () ()))

(define-metafunction formality-ty
  find-adt-generics : AdtId ((AdtId_1 Generics) ...) -> Generics

  [(find-adt-generics AdtId (_ ... (AdtId Generics) _ ...)) Generics]
  )

(define-extended-judgment-form formality-ty logic:prove-top-level-goal/cosld
  ;; See logic:prove-top-level-goal/cosld
  #:mode (ty:prove-top-level-goal/cosld I I O)
  #:contract (ty:prove-top-level-goal/cosld Env Goal Env)

  )

(define-syntax-rule (ty:test-can-prove env goal)
  (test-equal
   (judgment-holds (ty:prove-top-level-goal/cosld env goal _))
   #t)
  )

(define-syntax-rule (ty:test-cannot-prove env goal)
  (test-equal
   (judgment-holds (ty:prove-top-level-goal/cosld env goal _))
   #f)
  )

(define-metafunction formality-ty
  ;; Convenient metafunction for tests:
  ;;
  ;; Creates an environment introducing the various quantifiers etc and then the
  ;; given where-clauses (as hypotheses).
  ;;
  ;; Then proves the goal and extracts a "scheme".
  ;;
  ;; Returns the resulting scheme(s), which you can test with `test-match`.
  ty:prove-scheme : Env ((Quantifier KindedVarIds) ...) WhereClauses Goal -> Schemes

  [(ty:prove-scheme Env () WhereClauses Goal)
   (extract-schemes Envs_out Goal)
   (where/error Env_h (env-with-hypotheses Env (where-clauses->hypotheses-from-env Env WhereClauses)))
   (where/error Envs_out ,(judgment-holds (ty:prove-top-level-goal/cosld Env_h Goal Env_out) Env_out))
   ]

  [(ty:prove-scheme Env ((Quantifier_0 KindedVarIds_0) (Quantifier KindedVarIds) ...) WhereClauses Goal)
   (ty:prove-scheme Env_out ((Quantifier KindedVarIds) ...) WhereClauses_out Goal_out)
   (where/error (Env_out (WhereClauses_out Goal_out) _) (instantiate-quantified Env (Quantifier_0 KindedVarIds_0 (WhereClauses Goal))))
   ]

  )

(define-metafunction formality-ty
  ;; Converts a where-clause into a `Goal∧Clause`.
  ;;
  ;; The REAL version of this is in formality-decl; this is a "mock" version for
  ;; testing the typing code in isolation.
  where-clause->goal∧clause-mock : WhereClause -> Goal∧Clause

  [(where-clause->goal∧clause-mock (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds (where-clause->goal∧clause WhereClause))
   ]

  [(where-clause->goal∧clause-mock (Ty_self : TraitId[Parameter ...]))
   (is-implemented (TraitId (Ty_self Parameter ...)))
   ]

  [(where-clause->goal∧clause-mock (AliasTy == Ty))
   (normalizes-to AliasTy Ty)
   ]

  [(where-clause->goal∧clause-mock ((_ Parameter_a) : (_ Parameter_b)))
   (Parameter_a -outlives- Parameter_b)
   ]

  )