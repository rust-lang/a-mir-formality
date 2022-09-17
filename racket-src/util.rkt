#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

#;(current-traced-metafunctions '())
(define-syntax-rule (test-alpha-equivalent lang a b)
  (let ((alpha-equivalent (alpha-equivalent? lang a b)))
    (if alpha-equivalent (test-equal alpha-equivalent #t)
        (test-equal a b))))
(define-syntax-rule (test-equal-terms a b) (test-equal (term a) (term b)))
(define-syntax-rule (test-match-terms l a b) (test-match l b (term a)))
(define-syntax-rule (log name value) (begin (pretty-print (term ("entering" name))) (let [(v value)] (pretty-print (term ("exiting" name "with result" ,v))) v)))
(define-syntax-rule (log-value value) (let ((v value)) (begin (pretty-print v) v)))
(define-syntax-rule (log-term value) (let ((v (term value))) (begin (pretty-print v) v)))
(define (partition-list f l) (let-values [((matches matches-not) (partition f l))] (list matches matches-not)))
(define-syntax-rule (test-judgment-false j) (test-equal (judgment-holds j) #f))

(define-syntax-rule (traced l a) (begin (current-traced-metafunctions l)
                                        (let ((result a))
                                          (current-traced-metafunctions '())
                                          result)))

(define-syntax-rule (test-term-true a)
  (test-equal (term a) #t))

(define-syntax-rule (test-term-false a)
  (test-equal (term a) #f))

(define (stringify terms)
  ;; Useful function for making a list of terms into a deterministic order.
  ;; Converts a list of terms into a list of pretty-printed strings, sorted.
  (let* [(pretty-printed-terms (map pretty-format terms))
         (sorted-pretty-printed-terms (sort pretty-printed-terms string<?))
         ]
    pretty-printed-terms
    ))