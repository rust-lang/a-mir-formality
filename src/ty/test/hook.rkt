#lang racket

(require redex/reduction-semantics
         "../grammar.rkt"
         "../../logic/cosld-solve.rkt"
         "../../logic/env.rkt"
         "../../logic/instantiate.rkt"
         "../hook.rkt"
         "../relate.rkt"
         "../scheme.rkt"
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
                            (formality-ty-hook (lambda (predicate) (term Clauses))
                                               (term Invariants)
                                               (lambda (env predicate1 predicate2)
                                                 (term (ty:equate-predicates ,env ,predicate1 ,predicate2)))
                                               (lambda (env relation)
                                                 (term (ty:relate-parameters ,env ,relation)))
                                               (lambda (predicate1 predicate2) #t)
                                               (lambda (goal)
                                                 (term (ty:is-predicate? ,goal)))
                                               (lambda (goal)
                                                 (term (ty:is-relation? ,goal)))
                                               (lambda (adt-id)
                                                 (term (find-adt-generics ,adt-id ((AdtId Generics) ...))))
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
  ;; Creates an environment introducing the various quantifiers etc.
  ;; Then proves the goal and extracts a "scheme".
  ;; Returns the resulting scheme(s), which you can test with `test-match`.
  ty:prove-scheme : Env ((Quantifier KindedVarIds) ...) Goal -> Schemes

  [(ty:prove-scheme Env () Goal)
   (extract-schemes Envs_out Goal)
   (where/error Envs_out ,(judgment-holds (ty:prove-top-level-goal/cosld Env Goal Env_out) Env_out))
   ]

  [(ty:prove-scheme Env ((Quantifier_0 KindedVarIds_0) (Quantifier KindedVarIds) ...) Goal)
   (ty:prove-scheme Env_out ((Quantifier KindedVarIds) ...) Goal_out)
   (where/error (Env_out Goal_out _) (instantiate-quantified Env (Quantifier_0 KindedVarIds_0 Goal)))
   ]

  )
