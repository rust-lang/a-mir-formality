#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         )
(provide place-set-add
         place-set-remove-any-suffix
         place-is-prefix-of?
         place-set-contains-intersecting-place?
         place-set-contains-prefix-of?
         place-set-contains-suffix-of?
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
  ;; Removes a place `Place` from the place-set, along with any places that are a suffix of `Place`.
  ;;
  ;; Examples:
  ;;
  ;; 1. `{x} - y = {x}`
  ;; 2. `{x, y} - y = {x}`
  ;; 3. `{x, y.f1, y.f2} - y = {x}`
  ;; 4. `{x, y} - y.f1 - y.f2 = {x, y}`
  place-set-remove-any-suffix : Places Place -> Places

  [(place-set-remove-any-suffix Places Place_new)
   [Place_old1 ... ...]
   (where [Place_old ...] Places)
   (where [[Place_old1 ...] ...] [(place-if-not-prefixed-by Place_old Place_new) ...])
   ]
  )

(define-metafunction formality-body
  ;; True if any member of the place-set is a prefix of the given place.
  place-set-contains-prefix-of? : Places Place -> boolean

  [(place-set-contains-prefix-of? Places Place)
   (any? (place-is-prefix-of? Place_member Place) ...)
   (where/error [Place_member ...] Places)
   ]
  )

(define-metafunction formality-body
  ;; True if any member of the place-set is a suffix of the given place.
  place-set-contains-suffix-of? : Places Place -> boolean

  [(place-set-contains-suffix-of? Places Place)
   (any? (place-is-prefix-of? Place Place_member) ...)
   (where/error [Place_member ...] Places)
   ]
  )

(define-metafunction formality-body
  ;; True if any member of the place-set is either a prefix or a suffix of the given place.
  place-set-contains-intersecting-place? : Places Place -> boolean

  [(place-set-contains-intersecting-place? Places Place)
   (any? (place-set-contains-prefix-of? Places Place)
         (place-set-contains-suffix-of? Places Place))
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

  (test-equal
   (term (place-set-remove-any-suffix [x] x))
   (term []))

  (test-equal
   (term (place-set-remove-any-suffix [x y] x))
   (term [y]))

  (test-equal
   (term (place-set-remove-any-suffix [x (field y f1) (field y f2)] y))
   (term [x]))

  (test-equal
   (term (place-set-remove-any-suffix [x y] (field x f1)))
   (term [x y]))

  (test-equal
   (term (place-set-contains-intersecting-place? [x (field y f1)] x))
   (term #t))

  (test-equal
   (term (place-set-contains-intersecting-place? [x (field y f1)] y))
   (term #t))

  (test-equal
   (term (place-set-contains-intersecting-place? [x (field y f1)] (field y f1)))
   (term #t))

  (test-equal
   (term (place-set-contains-intersecting-place? [x (field y f1)] (field y f2)))
   (term #f))

  (test-equal
   (term (place-set-contains-intersecting-place? [x (field y f1)] z))
   (term #f))

  )