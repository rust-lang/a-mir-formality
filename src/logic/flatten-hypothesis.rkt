#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "elaborate.rkt"
         "env.rkt"
         )
(provide flattened-hypotheses-in-env
         )

(define-metafunction formality-logic
  ;; Given an Env, yields a flattened, elaborated view of its hypotheses.
  ;; Useful for matching.
  flattened-hypotheses-in-env : Env -> FlatHypotheses

  [(flattened-hypotheses-in-env Env)
   ((flatten-hypothesis () () Hypothesis) ...)

   (where/error (Hypothesis ...) (env-hypotheses (elaborate-hypotheses Env)))
   ]

  )

(define-metafunction formality-logic
  flatten-hypothesis : KindedVarIds Goals Hypothesis -> FlatHypothesis

  [(flatten-hypothesis KindedVarIds Goals AtomicGoal)
   (∀ KindedVarIds (implies Goals AtomicGoal))]

  [(flatten-hypothesis KindedVarIds (Goal_0 ...) (implies (Goal_1 ...) AtomicGoal))
   (flatten-hypothesis KindedVarIds (Goal_0 ... Goal_1 ...) AtomicGoal)
   ]

  [(flatten-hypothesis (KindedVarId_0 ...) Goals (∀ (KindedVarId_1 ...) Hypothesis))
   (flatten-hypothesis (KindedVarId_0 ... KindedVarId_1 ...) Goals Hypothesis)
   ]

  )

(define-metafunction formality-logic
  match-hypothesis : FlatHypothesis AtomicGoal -> (FlatImplicationHypothesis) or ()

  [(match-hypothesis (∀ ((ParameterKind_h VarId_h) ...) (implies Goals_h AtomicGoal_h)) AtomicGoal)
   ()

   (where Error (match-terms (VarId_h ...) AtomicGoal_h AtomicGoal))
   ]

  [(match-hypothesis (∀ ((ParameterKind_h VarId_h) ...) (implies Goals_h AtomicGoal_h)) AtomicGoal)
   (implies (apply-substitution Substitution Goals_h) (apply-substitution Substitution AtomicGoal_h))

   (where Substitution (match-terms (VarId_h ...) AtomicGoal_h AtomicGoal))
   ]

  )