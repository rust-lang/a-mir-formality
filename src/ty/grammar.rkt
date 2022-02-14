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
      TyNotVar        ; Everything else
      VarId           ; Existential or bound variable
      )
  (TyNotVar := (TyApply TyName Parameters))
  (TyName :=
          AdtId           ; enum/struct/union
          VarId           ; Universal placeholder
          TraitId         ; trait
          (TraitId AssociatedTyId) ; Associated type
          ScalarId        ; Something like i32, u32, etc
          (Ref MaybeMut)  ; `&mut` or `&`, expects a lifetiome + type parameter
          (Tuple number)  ; tuple of given arity
          )

  ;; Lt -- Rust lifetimes
  ;;
  ;; Very similar to types `Ty` in terms of how they are represented
  ;; and the meaning of `VarId`.
  (Lt :=
      LtNotVar
      VarId)
  (LtNotVar := static (LtApply LtName))
  (LtName := VarId)

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

  ;; MaybeMut: either mut or not
  (MaybeMut := () (mut) )

  ;; Substitution: map from bound variable `X`
  ;; to `Parameter` P with which `X` should be
  ;; replaced.
  ;;
  ;; See `substitution.rkt` for helper functions
  (Substitution-e := Error Substitution)
  (Substitution := ((VarId Parameter) ...))

  ;; KindedVarId: declares a bound parameter and
  ;; its kind (type, lifetime, etc).
  (KindedVarIds := (KindedVarId ...))
  (KindedVarId := (VarKind VarId))

  ;; VarKind: kind for a bound variable (type,
  ;; lifetime, etc)
  (VarKind := TyVar LtVar)

  ;; Parameter: value for a generic parameter
  (Parameters := (Parameter ...))
  (Parameter := Ty Lt)

  ;; Pairs of parameters
  (ParameterPairs := (ParameterPair ...))
  (ParameterPair := (Ty Ty) (Lt Lt))

  ;; `Predicate` -- the atomic items that we can prove
  (Predicate :=
             ; `TraitRef` is (fully) implemented.
             (Implemented TraitRef)
             ; an impl exists for `TraitRef`; this *by itself* doesn't mean
             ; that `TraitRef` is implemented, as the supertraits may not
             ; have impls.
             (HasImpl TraitRef)
             ; the given type is well-formed.
             (WellFormed ty)
             )

  ;; `Goal` -- things we can prove. These consists of predicates
  ;; joined by various forms of logical operators that are built
  ;; into the proving system (see `solve.rkt`).
  (Goals = (Goal ...))
  (Goal :=
        Predicate
        (All Goals)
        (Any Goals)
        (Implies Hypotheses Goal)
        (Quantifier KindedVarIds Goal)
        )

  ;; `Hypothesis` -- things that are assumed to be true.
  ;; These consists of simple predicates, higher-ranked hypotheses,
  ;; and implications. Implications are from implied bounds.
  (Hypotheses = (Hypothesis ...))
  (Hypothesis :=
              Predicate
              (Implies predicate predicate)
              (ForAll kinded-var-ids hypothesis)
              )

  ;; `Clause` -- axioms. These are both built-in and derived from
  ;; user-defined items like `trait` and `impl`.
  (Clauses := (Clause ...))
  (Clause :=
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

  ; Ids -- identifiers
  (VarIds := (VarId ...))
  ((AdtId
    ScalarId
    VarId
    TraitId
    AssociatedTyId) variable-not-otherwise-mentioned)

  ; Term -- preferred name to any that reads better :)
  (Term := any)

  #:binding-forms
  (ForAll ((VarKind VarId) ...) any #:refers-to (shadow VarId ...))
  (Exists ((VarKind VarId) ...) any #:refers-to (shadow VarId ...))
  )

(define-term
  RootUniverse
  (UniverseId 0)
  )

(define-term
  EmptyEnv
  (RootUniverse () () ())
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
  ;; Returns an `Env` where `VarId` is rebound to be in the given `Universe`;
  ;; this must be some smaller universe than it was in before.
  env-with-rebound-universe : Env VarId Universe -> Env

  [(env-with-rebound-universe Env VarId Universe_new)
   (Universe (VarUniverse_0 ... (VarId Quantifier Universe_new VarUniverse_1 ...) Clauses Hypotheses))
   (where/error (Universe (VarUniverse_0 ... (VarId Quantifier Universe_old) VarUniverse_1 ...) Clauses Hypotheses) Env)
   (where/error #t (universe-can-see Universe_old Universe_new))
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
   (where/error (_ ... (VarId Universe) _ ...) (env-var-universes Env))]

  [; Default is to consider random type names as "forall" constants in root universe
   (universe-of-var-in-env Env VarId)
   RootUniverse]

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

  [(free-variables (Quantifier ((VarKind VarId_bound) ...) any))
   ,(set-subtract (term VarIds_free) (term (VarId_bound ...)))
   (where/error VarIds_free (free-variables any))]

  [(free-variables VarId)
   (VarId)]

  [(free-variables (any ...))
   ,(apply set-union (term (() VarIds ...)))
   (where/error (VarIds ...) ((free-variables any) ...))
   ]

  [(free-variables _)
   ()]

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
  [(all? (true ...)) true]
  [(all? _) false]
  )

(define-metafunction formality-ty
  ;; Boolean operator
  any? : (boolean ...) -> boolean
  [(any? (_ ... true _ ...)) true]
  [(any? _) false]
  )

(define-metafunction formality-ty
  ;; Returns the smallest of the various universes provided
  min-universe : Universe ... -> Universe
  [(min-universe Universe) Universe]
  [(min-universe (UniverseId number) ...)
   (UniverseId ,(apply min (term (number ...))))
   ])

(define-metafunction formality-ty
  ;; True if `Universe_0` can see all values from `Universe_1`
  universe-can-see : Universe_0 Universe_1 -> boolean
  [(universe-can-see (UniverseId number_0) (UniverseId number_1))
   ,(>= (term number_0) (term number_1))])

(module+ test
  (test-match formality-ty
              Goal
              (term (All ())))
  (test-match formality-ty
              AdtId
              (term somevar))
  (test-match formality-ty
              Goal
              (term (Implemented (debug (i32)))))

  (test-equal (term (appears-free
                     x (ForAll ((TyVar x)) (x y))))
              #f)

  (test-equal (term (appears-free
                     y (ForAll ((TyVar x)) (x y))))
              #t)

  (test-equal (term (free-variables
                     (ForAll ((TyVar x) (TyVar z)) (a b c x y (d e f) g h (z w) ()))))
              (term (a b c y f e d g h w)))

  (test-equal (term (binding-in-env
                     (env-with-fresh-binding EmptyEnv (x ForAll (UniverseId 22)))
                     x))
              (term (ForAll (UniverseId 22))))

  (test-equal (term (min-universe (UniverseId 3) (UniverseId 5) (UniverseId 2)))
              (term (UniverseId 2)))

  )