#lang racket
(require redex/reduction-semantics "grammar.rkt")
(provide program-clauses-and-invariants
         formality-hook
         formality-ty-hook
         env-clauses-for-predicate
         env-invariants
         EmptyEnv
         env-with-clauses-and-invariants)

(define-metafunction formality-ty
  ;; Given the hook from the environment and the predicate we are solving,
  ;; yield up relevant
  program-clauses-and-invariants : Hook Predicate -> (Clauses Invariants)

  [(program-clauses-and-invariants (Hook: any_hook-fn) Predicate)
   ,(apply (term any_hook-fn) (term (Predicate)))
   ]
  )

(define-metafunction formality-ty
  ;; Returns all program clauses in the environment that are
  ;; potentially relevant to solving `Predicate`
  env-clauses-for-predicate : Env Predicate -> Clauses

  [(env-clauses-for-predicate Env Predicate)
   ,(let ((clauses-fn (formality-hook-clauses (term any))))
      (clauses-fn (term Predicate)))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-ty
  ;; Returns the invariants in the environment
  env-invariants : Env -> Clauses

  [(env-invariants Env)
   ,(formality-hook-invariants (term any))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(struct formality-hook (clauses invariants))

;; A suitable hook for testing the formality-ty layer in isolation:
;; just yields up a hardcoded list of clauses and invariants.
(define-metafunction formality-ty
  formality-ty-hook : Clauses Invariants -> Hook

  [(formality-ty-hook Clauses Invariants)
   (Hook: ,(formality-hook (lambda (predicate) (term Clauses))
                           (term Invariants)))
   ]
  )


;; A suitable env for testing the formality-ty layer in isolation
;; with no clauses nor invariants.
(define-term EmptyEnv (empty-env-with-hook (formality-ty-hook () ())))

(define-metafunction formality-ty
  ;; Creates a suitable env for testing the formality-ty layer
  ;; that has the given clauses + invariants as the program.
  env-with-clauses-and-invariants : Clauses Invariants -> Env

  [(env-with-clauses-and-invariants Clauses Invariants)
   (empty-env-with-hook (formality-ty-hook Clauses Invariants))
   ]
  )