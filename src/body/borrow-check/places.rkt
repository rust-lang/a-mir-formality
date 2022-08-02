#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         )
(provide
 )

(define-metafunction formality-body
  ;; Adds a place to a place set; if the place, or some more general place, is already present,
  ;; then returns the set without any changes.
  ;;
  ;; Examples:
  ;;
  ;; 1. `{x} + y = {x, y}`
  ;; 2. `{x, y} + y = {x, y}`
  ;; 3. `{x, y.f1, y.f2} + y = {x, y}`
  ;; 4. `{x, y} + y.f1 + y.f2 = {x, y}`
  place-set-add : Places Place -> Places

  [; If `Place` is already in the set, or something in the set is a prefix of `Place`,
   ; return `Places` without any changes (e.g., cases 2 and 4 above).
   (place-set-add Places Place_new)
   Places
   (where [_ ... Place_old _ ...] Places)
   (where #t (place-is-prefix-of? Place_old Place_new))
   ]

  [; If `Place` is already in the set, or something in the set is a prefix of `Place`,
   ; return `Places` without any changes (e.g., cases 2 and 4 above).
   (place-set-add Places Place_new)
   [Place_old1 ... ... Place_new]
   (where [Place_old ...] Places)
   (where [[Place_old1 ...] ...] [(place-if-not-prefixed-by Place_old Place_new) ...])
   ]
  )

(define-metafunction formality-body
  ;; If `Place_1` is not prefixed by `Place_2`, return `[Place_1]`.
  ;; Otherwise, returns `[]`.
  place-if-not-prefixed-by : Place Place -> [Place ...]

  [(place-if-not-prefixed-by Place_1 Place_2)
   [Place_1]
   (where #f (place-is-prefix-of? Place_2 Place_1))]

  [(place-if-not-prefixed-by Place_1 Place_2)
   []
   (where #t (place-is-prefix-of? Place_2 Place_1))]

  )

(define-metafunction formality-body
  ;; Adds a place to a place set; if the place, or some more general place, is already present,
  ;; then returns the set without any changes.
  ;;
  ;; Examples:
  ;;
  ;; * `{x} + y = {x, y}`
  ;; * `{x, y} + y = {x, y}`
  ;; * `{x, y.f1, y.f2} + y = {x, y}`
  ;; * `{x, y} + y.f1 + y.f2 = {x, y}`
  place-is-prefix-of? : Place_0 Place_1 -> boolean

  [(place-is-prefix-of? Place Place) #t]

  [(place-is-prefix-of? Place LocalId) #f]

  [(place-is-prefix-of? Place (* Place_1))
   (place-is-prefix-of? Place Place_1)
   ]

  [(place-is-prefix-of? Place (field Place_1 _))
   (place-is-prefix-of? Place Place_1)
   ]

  [(place-is-prefix-of? Place (index Place_1 _))
   (place-is-prefix-of? Place Place_1)
   ]

  [(place-is-prefix-of? Place (downcast Place_1 _))
   (place-is-prefix-of? Place Place_1)
   ]
  )

(module+ test

  (test-equal
   (term (place-set-add [x] x))
   (term [x]))

  (test-equal
   (term (place-set-add [x y] x))
   (term [x y]))

  (test-equal
   (term (place-set-add [x (field y f1) (field y f2)] y))
   (term [x y]))

  (test-equal
   (term (place-set-add [x y] (field x f1)))
   (term [x y]))

  )