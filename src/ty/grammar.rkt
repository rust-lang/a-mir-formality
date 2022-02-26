#lang racket
(require redex/reduction-semantics racket/set)
(provide (all-defined-out))

; Naming convention:
;
; * Rust keywords use the Rust keyword
; * Nonterminals and variables use CamelCase, as do random VariantNames not part of Rust syntax,
;   like TyAdt

(define-language formality-ty

  ;; Ty -- Rust types
  ;;
  ;; A type `Ty` can be a `VarId`, in which case it is
  ;; either an existential (inference) variable or a bound
  ;; variable. It can also be a `TyNotVar`, in which case
  ;; it is a known type.
  ;;
  ;; Generic types are represented using `TyApply` inside their
  ;; scope. So for example given `fn foo<T>() { ... }`,
  ;; inside the body of `foo`, we would represent `T` with
  ;; `(TyApply T ())`.
  (Tys := (Ty ...))
  (Ty :=
      (TyApply TyName Parameters) ; Application type
      VarId                       ; Bound or existential (inference) variable
      (! VarId)                   ; Universal (placeholder) variable
      )
  (TyName :=
          AdtId           ; enum/struct/union
          TraitId         ; trait
          AssociatedTy    ; Associated type
          ScalarId        ; Something like i32, u32, etc
          (Ref MaybeMut)  ; `&mut` or `&`, expects a lifetiome + type parameter
          (Tuple number)  ; tuple of given arity
          )

  ;; Lt -- Rust lifetimes
  ;;
  ;; Very similar to types `Ty` in terms of how they are represented
  ;; and the meaning of `VarId`.
  (Lt :=
      static                      ; 'static
      VarId                       ; Bound or existential (inference) variable
      (! VarId)                   ; Universal (placeholder) variable
      )

  ;; EnvSubstitution - pair of an environment + substitution. This is the output
  ;; from proving things.
  (EnvSubstitution := (Env Substitution))

  ;; Env: Typing environment
  ;;
  ;; * Universe -- the largest universe yet defined
  ;; * VarUniverses -- maps variable names to universes
  ;;   * When bound variables are instantiated, their names
  ;;     are added to this list.
  ;;   * When equating (existential) variables,
  ;;     we modify the universe that it is mapped to here
  ;; * Clauses -- program clauses derived from impls present
  ;;   or other such constructs
  ;; * Hypotheses -- facts believed to be true, introduced by
  ;;   where clauses
  (Env := (Universe VarUniverses Clauses Hypotheses))
  (Env-e := Env Error)

  ;; VarUniverse -- maps a `VarId` to a `Universe`
  (VarUniverses := (VarUniverse ...))
  (VarUniverse := (VarId Universe))

  ;; TraitRef = reference to a trait
  (TraitRef := (TraitId Parameters))

  ;; TraitRef = reference to a trait
  (AssociatedTy := (TraitId AssociatedTyId))
  (AssociatedTyApplication := (AssociatedTy Parameters))

  ;; MaybeMut: either mut or not
  (MaybeMut := () (mut) )

  ;; Substitution: map from bound variable `X`
  ;; to `Parameter` P with which `X` should be
  ;; replaced.
  ;;
  ;; See `substitution.rkt` for helper functions
  (Substitution-e := Error Substitution)
  (Substitution := (VarParameter ...))
  (VarParameter = (VarId Parameter))

  ;; KindedVarId: declares a bound parameter and
  ;; its kind (type, lifetime, etc).
  (KindedVarIds := (KindedVarId ...))
  (KindedVarId := (ParameterKind VarId))

  ;; ParameterKind: kind for a bound variable (type,
  ;; lifetime, etc)
  (ParameterKind := TyKind LtKind)

  ;; Parameter: value for a generic parameter
  (Parameters := (Parameter ...))
  (Parameter := Ty Lt)

  ;; Pairs of parameters
  (TermPairs := (TermPair ...))
  (TermPair := (Term Term))

  ;; `Predicate` -- the atomic items that we can prove
  (Predicate :=
             ; `TraitRef` is (fully) implemented.
             (Implemented TraitRef)
             ; an impl exists for `TraitRef`; this *by itself* doesn't mean
             ; that `TraitRef` is implemented, as the supertraits may not
             ; have impls.
             (HasImpl TraitRef)
             ; the given type or lifetime is well-formed.
             (WellFormed (ParameterKind Parameter))
             )

  ;; `Goal` -- things we can prove. These consists of predicates
  ;; joined by various forms of logical operators that are built
  ;; into the proving system (see `solve.rkt`).
  (Goals = (Goal ...))
  (Goal :=
        Predicate
        (Equate Term Term)
        (All Goals)
        (Any Goals)
        (Implies Hypotheses Goal)
        (Quantifier KindedVarIds Goal)
        (Hypothesized Goal)
        )

  ;; `Clause` -- axioms. These are both built-in and derived from
  ;; user-defined items like `trait` and `impl`.
  ((Hypotheses Clauses) := (Clause ...))
  ((Hypothesis Clause) :=
                       Predicate
                       (Implies Goals Predicate)
                       (ForAll KindedVarIds Clause)
                       )

  ;; `Quantifier` -- the two kinds of quantifiers.
  (Quantifier := ForAll Exists)

  ;; `Universe` -- the root universe `RootUniverse` consists of all user-defined names.
  ;; Each time we enter into a `ForAll` quantifier, we introduce a new universe
  ;; that extends the previous one to add new names that didn't exist in the old
  ;; universe (e.g., the placeholders for the universally quantified variables).
  ;; See the paper XXX
  (Universe := (UniverseId number))

  ;; Scalars -- numbers, booleans
  (ScalarId := i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 bool)

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  (VarIds := (VarId ...))
  ((AdtId

    VarId
    TraitId
    AssociatedTyId
    AnyId) variable-not-otherwise-mentioned)

  ; Term -- preferred name to any that reads better :)
  (Term := any)

  #:binding-forms
  (ForAll ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  (Exists ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  )

(define-term
  RootUniverse
  (UniverseId 0)
  )

(define-term
  EmptyEnv
  (RootUniverse () () ())
  )

(define-term
  TyUnit
  (TyApply (Tuple 0) ())
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  env-hypotheses : Env -> Hypotheses

  [(env-hypotheses (Universe VarUniverses Clauses Hypotheses)) Hypotheses]
  )

(define-metafunction formality-ty
  ;; Returns the program clauses in the environment
  env-clauses : Env -> Hypotheses

  [(env-clauses (Universe VarUniverses Clauses Hypotheses)) Clauses]
  )


(define-metafunction formality-ty
  ;; Same environment but without any clauses (only hypotheses)
  env-without-clauses : Env -> Env

  [(env-without-clauses (Universe VarUniverses _ Hypotheses)) (Universe VarUniverses () Hypotheses)]
  )

(define-metafunction formality-ty
  ;; Returns the `VarId -> Universe` mapping from the environment
  env-var-universes : Env -> VarUniverses

  [(env-var-universes (Universe VarUniverses Clauses Hypotheses)) VarUniverses]
  )

(define-metafunction formality-ty
  ;; Returns the current maximum universe in the environment
  env-universe : Env -> Universe

  [(env-universe (Universe VarUniverses Clauses Hypotheses)) Universe]
  )

(define-metafunction formality-ty
  ;; Extend `Env`, mapping the names in `VarIds` to the current universe
  env-with-vars-in-current-universe : Env VarIds -> Env

  [(env-with-vars-in-current-universe Env (VarId ...))
   (Universe ((VarId Universe) ... VarUniverse ...) Clauses Hypotheses)
   (where/error (Universe (VarUniverse ...) Clauses Hypotheses) Env)
   ]
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  env-with-hypotheses : Env Hypotheses -> Env

  [(env-with-hypotheses (Universe VarUniverses Clauses (Hypothesis_0 ...)) (Hypothesis_1 ...))
   (Universe VarUniverses Clauses (Hypothesis_0 ... Hypothesis_1 ...))
   ]
  )

(define-metafunction formality-ty
  ;; Returns an `Env` where `VarId` is guaranteed to contain only elements from
  ;; `Universe`.
  env-with-var-limited-to-universe : Env VarId Universe -> Env

  [(env-with-var-limited-to-universe Env VarId Universe_max)
   (Universe (VarUniverse_0 ... (VarId Universe_new) VarUniverse_1 ...) Clauses Hypotheses)
   (where/error (Universe (VarUniverse_0 ... (VarId Universe_old) VarUniverse_1 ...) Clauses Hypotheses) Env)
   (where/error Universe_new (min-universe Universe_old Universe_max))
   ]
  )

(define-metafunction formality-ty
  ;; Returns an `Env` where each of the given `VarId`s is guaranteed to
  ;; contain only elements from `Universe`.
  env-with-vars-limited-to-universe : Env (VarId ...) Universe -> Env

  [(env-with-vars-limited-to-universe Env () Universe_max)
   Env]

  [(env-with-vars-limited-to-universe Env (VarId_0 VarId_1 ...) Universe_max)
   (env-with-vars-limited-to-universe Env (VarId_1 ...) Universe_max)
   (where (UniverseId 0) (universe-of-var-in-env Env VarId_0))]

  [(env-with-vars-limited-to-universe Env (VarId_0 VarId_1 ...) Universe_max)
   (env-with-vars-limited-to-universe Env_1 (VarId_1 ...) Universe_max)
   (where/error Env_1 (env-with-var-limited-to-universe Env VarId_0 Universe_max))
   ]
  )

(define-metafunction formality-ty
  ;; Returns an environment where the current universe is incremented, and returns
  ;; this new universe.
  env-with-incremented-universe : Env -> Env

  [(env-with-incremented-universe Env)
   (Universe_new VarUniverses Clauses Hypotheses)

   (where/error (Universe VarUniverses Clauses Hypotheses) Env)
   (where/error Universe_new (next-universe Universe))
   ]

  )

(define-metafunction formality-ty
  ;; Finds the declared universe of `VarId` in the given environment.
  ;;
  ;; If `VarId` is not found in the `Env`, returns the root universe. This is a useful
  ;; default for random user-given names like `i32` or `Vec`.
  universe-of-var-in-env : Env VarId -> Universe

  [(universe-of-var-in-env Env VarId)
   Universe
   (where (_ ... (VarId Universe) _ ...) (env-var-universes Env))]

  [; Default is to consider random type names as "forall" constants in root universe
   (universe-of-var-in-env Env VarId_!_1)
   RootUniverse
   (where ((VarId_!_1 Universe) ...) (env-var-universes Env))]

  )

(define-metafunction formality-ty
  ;; Finds the declared universe of `VarId` in the given environment.
  ;;
  ;; If `VarId` is not found in the `Env`, returns the root universe. This is a useful
  ;; default for random user-given names like `i32` or `Vec`.
  var-defined-in-env : Env VarId -> boolean

  [(var-defined-in-env Env VarId)
   #t
   (where (_ ... (VarId Universe) _ ...) (env-var-universes Env))]

  [(var-defined-in-env Env VarId)
   #f]

  )

(define-metafunction formality-ty
  ;; Increments a universe to return the next largest universe.
  next-universe : Universe -> Universe

  [(next-universe (UniverseId natural))
   (UniverseId ,(+ 1 (term natural)))]
  )

(define-metafunction formality-ty
  ;; True if the given variable appears free in the given term.
  appears-free : VarId any -> boolean

  [(appears-free VarId any)
   ,(not (alpha-equivalent? formality-ty (term any) (term any_1)))
   (where/error any_1 (substitute any VarId (TyApply VarId ())))
   ]
  )

(define-metafunction formality-ty
  ;; Returns the set of variables that appear free in the given term.
  free-variables : any -> (VarId ...)

  [(free-variables (Quantifier ((ParameterKind VarId_bound) ...) any))
   ,(set-subtract (term VarIds_free) (term (VarId_bound ...)))
   (where/error VarIds_free (free-variables any))]

  [(free-variables VarId)
   (VarId)]

  [; The `c` in `(TyApply c ())` is not a variable but a constant.
   (free-variables (TyApply _ Substitution))
   (free-variables Substitution)]

  [(free-variables (LtApply _))
   ()]

  [(free-variables (any ...))
   ,(apply set-union (term (() VarIds ...)))
   (where/error (VarIds ...) ((free-variables any) ...))
   ]

  [(free-variables _)
   ()]

  )

(define-metafunction formality-ty
  ;; Returns the set of names that are "placeholders", i.e.,
  ;; rigid types. For example, if you have `(TyApply Vec ((TyApply X ())))`,
  ;; this would return `(Vec X)`.
  placeholder-variables : Term -> (VarId ...)

  [(placeholder-variables (! VarId))
   (VarId)]

  [(placeholder-variables (Term ...))
   ,(apply set-union (term (() VarIds ...)))
   (where/error (VarIds ...) ((placeholder-variables Term) ...))
   ]

  [(placeholder-variables _)
   ()]

  )

(define-metafunction formality-ty
  ;; Returns the current maximum universe in the environment
  env-with-clauses-and-hypotheses : Env Clauses Hypotheses -> Env

  [(env-with-clauses-and-hypotheses (Universe VarUniverses (Clause_old ...) (Hypothesis_old ...)) (Clause_new ...) (Hypothesis_new ...))
   (Universe VarUniverses (Clause_old ... Clause_new ...) (Hypothesis_old ... Hypothesis_new ...))]
  )

(define-metafunction formality-ty
  ;; Boolean operator
  not? : boolean -> boolean
  [(not? false) true]
  [(not? true) false]
  )

(define-metafunction formality-ty
  ;; Boolean operator
  all? : (boolean ...) -> boolean
  [(all? (#t ...)) #t]
  [(all? _) #f]
  )

(define-metafunction formality-ty
  ;; Boolean operator
  any? : (boolean ...) -> boolean
  [(any? (_ ... #t _ ...)) #t]
  [(any? _) #f]
  )

(define-metafunction formality-ty
  ;; Returns the smallest of the various universes provided
  min-universe : Universe ... -> Universe
  [(min-universe (UniverseId number) ...)
   (UniverseId ,(apply min (term (number ...))))
   ])

(define-metafunction formality-ty
  ;; Returns the smallest of the various universes provided
  max-universe : Universe ... -> Universe
  [(max-universe (UniverseId number) ...)
   (UniverseId ,(apply max (term (number ...))))
   ])

(define-metafunction formality-ty
  ;; True if `Universe_0` includes all values of `Universe_1`
  universe-includes : Universe_0 Universe_1 -> boolean
  [(universe-includes (UniverseId number_0) (UniverseId number_1))
   ,(>= (term number_0) (term number_1))])


(define-metafunction formality-ty
  ;; Flatten a list of lists.
  flatten : ((Term ...) ...) -> (Term ...)

  [(flatten ((Term ...) ...)) (Term ... ...)]
  )

(define-metafunction formality-ty
  scalar-ty : ScalarId -> Ty

  ((scalar-ty ScalarId) (TyApply ScalarId ()))
  )

(module+ test
  (test-match formality-ty
              Goal
              (term (All ())))
  (test-match formality-ty
              AdtId
              (term somevar))
  (test-match formality-ty
              Goal
              (term (Implemented (Debug ((scalar-ty i32))))))

  (test-equal (term (appears-free
                     x (ForAll ((TyKind x)) (x y))))
              #f)

  (test-equal (term (appears-free
                     y (ForAll ((TyKind x)) (x y))))
              #t)

  (test-equal (term (free-variables
                     (ForAll ((TyKind x) (TyKind z)) (a b c x y (d e f) g h (z w) ()))))
              (term (a b c y f e d g h w)))

  (test-equal (term (min-universe (UniverseId 3) (UniverseId 5) (UniverseId 2)))
              (term (UniverseId 2)))

  (test-equal (term (max-universe (UniverseId 3) (UniverseId 5) (UniverseId 2)))
              (term (UniverseId 5)))

  )