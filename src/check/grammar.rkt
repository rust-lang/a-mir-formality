#lang racket
(require redex/reduction-semantics
         "../body/grammar.rkt"
         )
(provide (all-defined-out))

(define-extended-language formality-check formality-body
  )