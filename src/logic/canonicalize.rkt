#lang racket
(require redex/reduction-semantics
         racket/set
         "grammar.rkt"
         "env.rkt"
         "substitution.rkt"
         )
(provide canonicalize-term)

#;(define-metafunction formality-logic
    canonicalize-term : Env Term -> CanonicalTerm
    [(canonicalize-term Env Term_0)
     xxx
     (where/error Term_1 (apply-substitution-from-env Env Term_0))

     ; find the free variables in Term and the binding info about them from env
     (where/error (VarId_free ...) (free-variables Env Term_1))
     (where/error (VarBinder_free ...) ((var-binding-in-env Env VarId_free) ...))

     ; find the universes that these free variables refer to
     (where/error ((_ _ _ Universe_free) ...) (VarBinder_free ...))
     (where/error (Universe_all ...) (RootUniverse Universe_free ...))
     ]
    )

(define-metafunction formality-logic
  ;; Replace all universes in `Term` with their replacements from `UniversePairs`.
  ;;
  ;; Precondition: All universes appearing in `Term` must also be mapped by `UniversePairs`.
  apply-universe-map : UniversePairs Term -> Term

  [(apply-universe-map UniversePairs Universe)
   Universe_1
   (where/error (_ ... (Universe Universe_1) _ ...) UniversePairs)]

  [(apply-universe-map UniversePairs (Term ...))
   ((apply-universe-map UniversePairs Term) ...)]

  [(apply-universe-map UniversePairs Term)
   Term]
  )

(define-metafunction formality-logic
  ;; Convert a map `U0 -> U1` to `U1 -> U0`.
  reverse-universe-map : UniversePairs -> UniversePairs

  [(reverse-universe-map ((Universe_from Universe_to) ...))
   ((Universe_to Universe_from) ...)]
  )

(define-metafunction formality-logic
  ;; Given a set of universes extracted from some term that is being canonicalized,
  ;; returns a "universe map" that maps from each distinct universe to a new, more company
  ;; universe to be used in the canonical form. The new universes preserve the relative ordering
  ;; from the input.
  ;;
  ;; Example: given `(U0 U1 U1 U5)`, would return `((U0 U0) (U1 U1) (U5 U2))`; note that each
  ;; distinct universe from the input (`U0`, `U1`, `U5`) is mapped to a (consecutive) universe
  ;; such that their relative orderings are preserved.
  universe-map : Universes -> (UniversePair ...)

  ((universe-map Universes)
   (number-universes 0 Universes_sorted)
   (where/error Universes_dedup ,(set-union (term (RootUniverse)) (term Universes)))
   (where/error Universes_sorted ,(sort (term Universes_dedup) (lambda (a b) (term (universe-includes ,b ,a)))))
   )
  )

(define-metafunction formality-logic
  number-universes : number Universes -> (UniversePair ...)

  [(number-universes number ()) ()]

  [(number-universes number (Universe_0 Universe_1 ...))
   ((Universe_0 (universe number)) UniversePair ...)
   (where/error number_1 ,(+ (term number) 1))
   (where/error (UniversePair ...) (number-universes number_1 (Universe_1 ...)))
   ]
  )

(module+ test

  (test-equal (term (universe-map ((universe 1) (universe 1) (universe 3) (universe 7))))
              (term (((universe 0) (universe 0))
                     ((universe 1) (universe 1))
                     ((universe 3) (universe 2))
                     ((universe 7) (universe 3)))))

  (test-equal (term (apply-universe-map (((universe 0) (universe 1)) ((universe 1) (universe 0))) (a b (universe 0) (universe 1))))
              (term (a b (universe 1) (universe 0))))

  )