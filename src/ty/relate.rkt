#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "relate/outlives.rkt"
         "relate/subtype.rkt"
         "relate/equate.rkt"
         "../logic/substitution.rkt"
         )

(provide ty:relate-parameters
         )

(define-metafunction formality-ty
  ty:relate-parameters : Env Relation -> (Env Goals) or Error

  [(ty:relate-parameters Env (Parameter_1 == Parameter_2))
   (equate/one/substituted Env (apply-substitution-from-env Env (Parameter_1 == Parameter_2)))
   ]

  [(ty:relate-parameters Env (Parameter_1 SubtypeOp Parameter_2))
   (compare/one/substituted Env (apply-substitution-from-env Env (Parameter_1 SubtypeOp Parameter_2)))
   ]

  [(ty:relate-parameters Env (Parameter_1 OutlivesOp Parameter_2))
   (outlives/one/substituted Env (apply-substitution-from-env Env (Parameter_1 OutlivesOp Parameter_2)))
   ]
  )

