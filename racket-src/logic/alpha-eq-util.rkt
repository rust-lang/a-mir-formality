#lang racket
(require redex/reduction-semantics
         "env.rkt"
         "grammar.rkt")
(provide (all-defined-out))

(define-metafunction formality-logic
  ;; This is utility for writing tests where the return value are two sets.
  ;; Given two sets, it checks that each item in the first set has an
  ;; alpha-equivalent entry in the second, and vice-versa.
  ;;
  ;; If this is true, it returns `ok`.
  ;;
  ;; Otherwise, it returns `Terms_actual`
  ;;
  ;; The intended use is like this:
  ;;
  ;; (test-equal
  ;;   (term (test-alpha-equivalent-sets
  ;;          (.. something .. )
  ;;          (.. expected-result ..)
  ;;          ))
  ;;   'ok)
  ;;
  ;; Useful when the ordering that is returned is unstable.
  test-alpha-equivalent-sets : Terms_actual Terms_expected -> Term

  [(test-alpha-equivalent-sets [Term_actual ...] [Term_expected ...])
   ok
   (where [#t ...] [(in? Term_actual [Term_expected ...]) ...])
   (where [#t ...] [(in? Term_expected [Term_actual ...]) ...])
   ]

  [(test-alpha-equivalent-sets [Term_actual ...] [Term_expected ...])
   [Term_actual ...]
   ]
  )