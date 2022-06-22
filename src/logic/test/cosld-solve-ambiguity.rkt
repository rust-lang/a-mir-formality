#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../cosld-solve.rkt"
         "hook.rkt"
         )

(module+ test
  (define-metafunction formality-logic
    ;; Convenience judgment that extracts the Substitution from the Env for testing.
    proof-results : Env Goal -> [EnvOutput ...]

    [(proof-results Env Goal)
     ,(judgment-holds (logic:prove-top-level-goal/cosld Env Goal EnvOutput) EnvOutput)
     ]
    )

  (test-equal
   (term (proof-results EmptyEnv ambiguous))
   (term [ambiguous])
   )

  (; simple tests where ambiguity is input dependent
   redex-let*
   formality-logic
   [(Env (term (env-with-clauses-and-invariants
                [(implies [ambiguous] (ambig-if-p p))
                 (ambig-if-p q)
                 ]
                []
                )))]

   (test-equal
    (term (proof-results Env (ambig-if-p q)))
    (term [Env])
    )

   (test-match
    formality-logic
    [ambiguous]
    (term (proof-results Env (ambig-if-p p)))
    )

   (test-match
    formality-logic
    [Env_1 ambiguous]
    (term (proof-results Env (∃ [(var ?X)] (ambig-if-p ?X))))
    )
   )

  (; test where ambiguity happens in first clause (`ambig-if-p`) but then gets resolved
   ; by next clause (`p == q`)
   redex-let*
   formality-logic
   [(Env (term (env-with-clauses-and-invariants
                [(implies [ambiguous] (ambig-if-p p))
                 (ambig-if-p q)
                 (∀ [(var ?X)] (implies
                                [(ambig-if-p ?X)
                                 (?X == q)
                                 ]
                                (foo ?X)))
                 ]
                []
                )))]

   (test-match
    formality-logic
    []
    (term (proof-results Env (foo p)))
    )

   (test-match
    formality-logic
    [Env_1]
    (term (proof-results Env (foo q)))
    )

   (test-match
    formality-logic
    [Env_1]
    (term (proof-results Env (∃ [(var ?X)] (foo ?X))))
    )
   )
  )
