#lang racket
(require redex/reduction-semantics racket/set)
(provide (all-defined-out))

; Naming convention:
;
; * Rust keywords use the Rust keyword
; * Nonterminals and variables use CamelCase, as do random VariantNames not part of Rust syntax,
;   like TyAdt

(define-language formality-ty
  ;; The "hook" is a bit of a hack that allows the environment
  ;; to (on demand) get access to clauses and invariants from the
  ;; program without actually knowing the full representation
  ;; of the program itself. See hook.rkt for more details.
  (Hook ::= (Hook: any))

  ;; Ty -- Rust types
  ;;
  ;; A type `Ty` can be a `VarId`, in which case it is
  ;; either an existential (inference) variable or a bound
  ;; variable. It can also be a `TyNotVar`, in which case
  ;; it is a known type.
  ;;
  ;; Generic types are represented using `TyRigid` inside their
  ;; scope. So for example given `fn foo<T>() { ... }`,
  ;; inside the body of `foo`, we would represent `T` with
  ;; `(TyRigid T ())`.
  (Tys ::= (Ty ...))
  (Ty ::=
      (TyRigid RigidName Parameters) ; A *rigid* type is onee that can only be equal to itself.
      VarId                          ; Bound or existential (inference) variable
      (! VarId)                      ; Universal (placeholder) variable
      )
  (RigidName ::=
             AdtId           ; enum/struct/union
             TraitId         ; trait
             AssociatedTy    ; Associated type
             ScalarId        ; Something like i32, u32, etc
             (Ref MaybeMut)  ; `&mut` or `&`, expects a lifetime + type parameter
             (Tuple number)  ; tuple of given arity
             )

  ;; Lt -- Rust lifetimes
  ;;
  ;; Very similar to types `Ty` in terms of how they are represented
  ;; and the meaning of `VarId`.
  (Lt ::=
      static                      ; 'static
      VarId                       ; Bound or existential (inference) variable
      (! VarId)                   ; Universal (placeholder) variable
      )

  ;; Env: Typing environment
  ;;
  ;; * Hook -- the "hook" that lets us get program information
  ;; * Universe -- the largest universe yet defined
  ;; * VarBinders -- maps variable names to quantifier-kinds/universes
  ;;   * When bound variables are instantiated, their names
  ;;     are added to this list.
  ;;   * When equating (existential) variables,
  ;;     we modify the universe that it is mapped to here
  ;; * Hypotheses -- facts believed to be true, introduced by
  ;;   where clauses
  (Env ::= (Hook Universe VarBinders Substitution Hypotheses))

  ;; VarBinder -- maps a `VarId` to a quantifier kind and `Universe`
  (VarBinders ::= (VarBinder ...))
  (VarBinder ::= (VarId Quantifier Universe))

  ;; TraitRef = reference to a trait
  (TraitRef ::= (TraitId Parameters))

  ;; TraitRef = reference to a trait
  (AssociatedTy ::= (TraitId AssociatedTyId))
  (AssociatedTyApplication ::= (AssociatedTy Parameters))

  ;; MaybeMut: either mut or not
  (MaybeMut ::= () (mut) )

  ;; Substitution: map from bound variable `X`
  ;; to `Parameter` P with which `X` should be
  ;; replaced.
  ;;
  ;; See `substitution.rkt` for helper functions
  (Substitution ::= (VarParameter ...))
  (VarParameter = (VarId Parameter))

  ;; KindedVarId: declares a bound parameter and
  ;; its kind (type, lifetime, etc).
  (KindedVarIds ::= (KindedVarId ...))
  (KindedVarId ::= (ParameterKind VarId))

  ;; ParameterKind: kind for a bound variable (type,
  ;; lifetime, etc)
  (ParameterKind ::= TyKind LtKind)

  ;; Parameter: value for a generic parameter
  (Parameters ::= (Parameter ...))
  (Parameter ::= Ty Lt)

  ;; Pairs of parameters
  (TermPairs ::= (TermPair ...))
  (TermPair ::= (Term Term))

  ;; `Predicate` -- the atomic items that we can prove
  (Predicates ::= (Predicate ...))
  (Predicate ::=
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
  ;; into the proving system (see `cosld-solve.rkt`).
  (Goals = (Goal ...))
  (Goal ::=
        Predicate
        (Equate Term Term)
        (All Goals)
        (Any Goals)
        (Implies Hypotheses Goal)
        (Quantifier KindedVarIds Goal)
        )

  ;; `Clause`, `Hypothesis` -- axioms. These are both built-in and derived from
  ;; user-defined items like `trait` and `impl`.
  (Hypotheses Clauses ::= (Clause ...))
  (Hypothesis Clause ::=
              Predicate
              (Implies Goals Predicate)
              (ForAll KindedVarIds Clause)
              )

  ;; `Invariants` -- things which must be true or the type system has some bugs.
  ;; A rather restricted form of clause.
  (Invariants ::= (Invariant ...))
  (Invariant ::= (ForAll KindedVarIds (Implies (Predicate) Predicate)))

  ;; `Quantifier` -- the two kinds of quantifiers.
  (Quantifier ::= ForAll Exists)

  ;; `Universe` -- the root universe `RootUniverse` consists of all user-defined names.
  ;; Each time we enter into a `ForAll` quantifier, we introduce a new universe
  ;; that extends the previous one to add new names that didn't exist in the old
  ;; universe (e.g., the placeholders for the universally quantified variables).
  ;; See the paper XXX
  (Universe ::= (UniverseId number))

  ;; Scalars -- numbers, booleans
  (ScalarId ::= i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 bool)

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  (VarIds ::= (VarId ...))
  ((AdtId
    VarId
    TraitId
    AssociatedTyId
    AnyId) variable-not-otherwise-mentioned)

  ; Term -- preferred name to any that reads better :)
  (Term ::= any)

  #:binding-forms
  (ForAll ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  (Exists ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  )

(define-term
  RootUniverse
  (UniverseId 0)
  )

(define-term
  TyUnit
  (TyRigid (Tuple 0) ())
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  env-with-hook : Hook -> Env

  [(env-with-hook Hook)
   (Hook RootUniverse () () ())]
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  env-hook : Env -> Hook

  [(env-hook (Hook Universe VarBinders Substitution Hypotheses)) Hook]
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  env-hypotheses : Env -> Hypotheses

  [(env-hypotheses (Hook Universe VarBinders Substitution Hypotheses)) Hypotheses]
  )

(define-metafunction formality-ty
  ;; Returns the `VarId -> Universe` mapping from the environment
  env-var-binders : Env -> VarBinders

  [(env-var-binders (Hook Universe VarBinders Substitution Hypotheses)) VarBinders]
  )

(define-metafunction formality-ty
  ;; Returns the substitution from the environment -- i.e., the currently inferred values
  ;; for any existential variables
  env-substitution : Env -> Substitution

  [(env-substitution (Hook Universe VarBinders Substitution Hypotheses)) Substitution]
  )

(define-metafunction formality-ty
  ;; Returns the current maximum universe in the environment
  env-universe : Env -> Universe

  [(env-universe (Hook Universe VarBinders Substitution Hypotheses)) Universe]
  )

(define-metafunction formality-ty
  ;; Extend `Env`, mapping the names in `VarIds` to the current universe
  env-with-vars-in-current-universe : Env Quantifier VarIds -> Env

  [(env-with-vars-in-current-universe Env Quantifier (VarId ...))
   (Hook Universe ((VarId Quantifier Universe) ... VarBinder ...) Substitution Hypotheses)
   (where/error (Hook Universe (VarBinder ...) Substitution Hypotheses) Env)
   ]
  )

(define-metafunction formality-ty
  ;; Returns the hypotheses in the environment
  env-with-hypotheses : Env Hypotheses -> Env

  [(env-with-hypotheses Env ()) Env]

  [(env-with-hypotheses Env (Hypothesis_0 Hypothesis_1 ...))
   (env-with-hypotheses Env_1 (Hypothesis_1 ...))
   (where/error Env_1 (env-with-hypothesis Env Hypothesis_0))
   ]
  )

(define-metafunction formality-ty
  ;; Adds a hypothesis (if not already present)
  env-with-hypothesis : Env Hypothesis -> Env

  [(env-with-hypothesis Env Hypothesis_1)
   Env
   (where (_ ... Hypothesis_0 _ ...) (env-hypotheses Env))
   (where #t ,(alpha-equivalent? (term Hypothesis_0) (term Hypothesis_1)))
   ]

  [(env-with-hypothesis (Hook Universe VarBinders Substitution (Hypothesis_0 ...)) Hypothesis_1)
   (Hook Universe VarBinders Substitution (Hypothesis_0 ... Hypothesis_1))
   ]

  )

(define-metafunction formality-ty
  ;; Returns an `Env` where `VarId` is guaranteed to contain only elements from
  ;; `Universe`.
  env-with-var-limited-to-universe : Env VarId Universe -> Env

  [(env-with-var-limited-to-universe Env VarId Universe_max)
   (Hook Universe (VarBinder_0 ... (VarId Quantifier Universe_new) VarBinder_1 ...) Substitution Hypotheses)
   (where/error (Hook Universe (VarBinder_0 ... (VarId Quantifier Universe_old) VarBinder_1 ...) Substitution Hypotheses) Env)
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
   (Hook Universe_new VarBinders Substitution Hypotheses)

   (where/error (Hook Universe VarBinders Substitution Hypotheses) Env)
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
   (where (_ ... (VarId Quantifier Universe) _ ...) (env-var-binders Env))]

  [; Default is to consider random type names as "forall" constants in root universe
   (universe-of-var-in-env Env VarId_!_1)
   RootUniverse
   (where ((VarId_!_1 _ _) ...) (env-var-binders Env))]

  )


(define-metafunction formality-ty
  ;; True if this variable is an existential variable defined in the environment.
  env-contains-existential-var : Env VarId -> boolean

  [(env-contains-existential-var Env VarId)
   #t
   (where (_ ... (VarId Exists Universe) _ ...) (env-var-binders Env))]

  [(env-contains-existential-var Env VarId)
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
   (where/error any_1 (substitute any VarId (TyRigid VarId ())))
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

  [; The `c` in `(TyRigid c ())` is not a variable but a constant.
   (free-variables (TyRigid _ Substitution))
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
  ;; Returns the set of universally quantified variables from
  ;; within the term -- this excludes global constants like
  ;; adt names. So e.g. if you have `(TyRigid Vec ((! X)))`,
  ;; this would return `(X)`.
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
  ;; Boolean operator
  not? : boolean -> boolean
  [(not? #f) #t]
  [(not? #t) #f]
  )

(define-metafunction formality-ty
  ;; Boolean operator
  all? : boolean ... -> boolean
  [(all? #t ...) #t]
  [(all? _ ...) #f]
  )

(define-metafunction formality-ty
  ;; Boolean operator
  any? : boolean ... -> boolean
  [(any? _ ... #t _ ...) #t]
  [(any? _ ...) #f]
  )

(define-metafunction formality-ty
  ;; Boolean operator
  in? : Term (Term ...) -> boolean
  [(in? Term (_ ... Term _ ...)) #t]
  [(in? _ _) #f]
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

  ((scalar-ty ScalarId) (TyRigid ScalarId ()))
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
