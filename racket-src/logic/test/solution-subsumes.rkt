#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../env.rkt"
         "../solution-subsumes.rkt"
         "hook.rkt"
         )

(module+ test

  ; Solution A is "true for any `q` where `q == Foo`"
  ; Solution B is "true when `q = Foo`"
  ; We prefer Solution A.
  ;
  ; This is a weird test, but it is showing a general pattern that
  ; we would rather take a solution that adds a relation that must be proven
  ; (`q == Foo`) than force the value of a variable (`q`).
  ;
  ; A scenario where this arises in practice sometimes is `'a == 'body` vs
  ; `'a: 'body`. But we can't test that directly because the logic layer testing
  ; only understands one form of relation (`==`).
  (redex-let*
   formality-logic
   [(Hook (term (env-hook (env-with-clauses-and-invariants [] []))))
    (QueryGoal (term (?- [(q type ∃ (universe 1))] (implies [] true-goal))))
    (Solution_a (term (:- [] ([] [(q == Foo)]))))
    (Solution_b (term (:- [] ([(q Foo)] []))))
    ]

   (traced '()
           (test-equal
            (term (solution-subsumes Hook QueryGoal Solution_a Solution_b))
            #t))
   )

  ; Solution A is "always true"
  ; Solution B is "true if q == ?b", where `?b` is a fresh variable.
  ; We prefer solution A.
  ; Actually these solutions are equivalent, but we're not smart enough
  ; to see that `q == ?b` for a fresh variable `?b` is not a real constraint.
  ; This shortcoming is intended to be overcome by solution simplification,
  ; but another alternative would be making our rules for entailment smarter,
  ; which might in principle be done via more core axioms than hardcoding this idea.
  (redex-let*
   formality-logic
   [(Hook (term (env-hook (env-with-clauses-and-invariants [] []))))
    (QueryGoal (term (?- [(q type ∃ (universe 1))] (implies [] true-goal))))
    (Solution_a (term (:- [] ([] []))))
    (Solution_b (term (:- [(?b type ∃ (universe 1))] ([(q ?b)] []))))
    ]

   ; "Using" solution A (which adds nothing), we cannot prove solution B without
   ; further constraining some variables (notably `q`).
   (traced '()
           (test-equal
            (term (solution-entails Hook QueryGoal Solution_a Solution_b))
            #f
            ))

   ; Using solution B we CAN prove solution A.
   (traced '()
           (test-equal
            (term (solution-entails Hook QueryGoal Solution_b Solution_a))
            #t
            ))

   ; Therefore, solution A *subsumes* solution B.
   (traced '()
           (test-equal
            (term (solution-subsumes Hook QueryGoal Solution_a Solution_b))
            #t))
   )
  )