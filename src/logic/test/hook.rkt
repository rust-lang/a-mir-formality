#lang racket
(require redex/reduction-semantics "../grammar.rkt" "../env.rkt" "../hook.rkt" "unify.rkt")
(provide env-with-clauses-and-invariants
         EmptyEnv
         )

(define-metafunction formality-logic
  ;; Creates a suitable env for testing the formality-logic layer
  ;; that has the given clauses + invariants as the program.
  env-with-clauses-and-invariants : Clauses Invariants -> Env

  [(env-with-clauses-and-invariants Clauses Invariants)
   (env-with-hook (Hook: ,(begin
                            (formality-logic-hook
                             (lambda (predicate) (term Clauses))
                             (lambda () (term Invariants))
                             (lambda (env relation) (term (test-relate-parameters ,env ,relation)))
                             (lambda (goal) (term (logic:is-predicate? ,goal)))
                             (lambda (predicate) (term (logic:test-debone-predicate ,predicate)))
                             (lambda (goal) (term (logic:is-relation-goal? ,goal)))
                             ))))
   ]
  )

;; A suitable env for testing the formality-logic layer in isolation
;; with no clauses nor invariants.
(define-term EmptyEnv (env-with-clauses-and-invariants () ()))


(define-metafunction formality-logic
  ;; The "grammar" for predicates is just *any term* -- that's not very
  ;; useful, and extension languages refine it. When matching against predicates,
  ;; then, we can use this function to avoid matching on other kinds of goals.
  logic:is-predicate? : Goal -> boolean

  ;; Because the grammar term for `Parameter` is...rather expansive,
  ;; screen out relations / built-in stuff.
  [(logic:is-predicate? BuiltinGoal) #f]
  [(logic:is-predicate? (Parameter_1 == Parameter_2)) #f]

  ;; For predicate tests, we expect predicates to either be some kind of
  ;; fixed word (`p`) or else `(p Parameter ...)`.
  [(logic:is-predicate? VarId) #t]
  [(logic:is-predicate? (VarId Parameter ...)) #t]
  )

(define-metafunction formality-logic
  ;; For predicate tests, we expect predicates to either be some kind of
  ;; fixed word (`p`) or else `(p Parameter ...)`.
  logic:test-debone-predicate : Predicate -> Predicate/Deboned

  [(logic:test-debone-predicate VarId) (VarId ())]
  [(logic:test-debone-predicate (VarId Parameter ...)) (VarId (Parameter ...))]
  )

(define-metafunction formality-logic
  ;; At this layer, only `==` is a recognized relation
  logic:is-relation-goal? : Goal -> boolean

  [(logic:is-relation-goal? (Parameter_1 == Parameter_2)) #t]
  [(logic:is-relation-goal? _) #f]
  )