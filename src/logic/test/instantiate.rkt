#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../instantiate.rkt"
         "../../util.rkt"
         )
(module+ test
  (redex-let*
   formality-logic
   []

   (test-match-terms
    formality-logic
    (term (instantiate-quantified EmptyEnv (∀ ((Atom V)) (Vec (V)))))
    (term (_ (Vec (VarId_V1)) (VarId_V1))))

   (test-match-terms
    formality-logic
    (term (instantiate-quantified EmptyEnv (∃ ((Atom V)) (Vec (V)))))
    (term (_ (Vec (VarId_V1)) (VarId_V1))))
   )
  )