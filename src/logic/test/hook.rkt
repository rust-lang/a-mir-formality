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
                             (lambda (env predicate) (term Clauses))
                             (lambda () (term Invariants))
                             (lambda (env relation) (term (test-relate-parameters ,env ,relation)))
                             (lambda (env predicate) (term Error)) ; no built-in predicates
                             (lambda (predicate) (term (logic:test-debone-predicate ,predicate)))
                             (lambda (goal) (term (logic:categorize-goal ,goal)))
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
  logic:categorize-goal : Goal -> Goal/Categorization

  [(logic:categorize-goal BuiltinGoal) builtin-goal]

  [(logic:categorize-goal (Parameter_1 == Parameter_2)) builtin-relation]

  ;; For predicate tests, we expect predicates to either be some kind of
  ;; fixed word (`p`), a word with inputs `(p Parameter ...)`, or
  ;; a word with inputs and outputs `(p Parameters -> Parameters)`.
  [(logic:categorize-goal VarId) user-predicate]
  [(logic:categorize-goal (VarId Parameters_in -> Parameters_out)) user-predicate]
  [(logic:categorize-goal (VarId Parameter ...)) user-predicate]
  )

(define-metafunction formality-logic
  ;; For predicate tests, we expect predicates to either be some kind of
  ;; fixed word (`p`) or else `(p Parameter ...)`.
  logic:test-debone-predicate : Predicate -> Predicate/Deboned

  [(logic:test-debone-predicate VarId) (VarId () -> ())]
  [(logic:test-debone-predicate (VarId Parameters_in -> Parameters_out)) (VarId Parameters_in -> Parameters_out)]
  [(logic:test-debone-predicate (VarId Parameter ...)) (VarId (Parameter ...) -> ())]
  )