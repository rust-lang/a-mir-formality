#lang racket
(require redex/reduction-semantics racket/set)
(provide (all-defined-out))

; Naming convention:
;
; * Rust keywords use the Rust keyword
; * Nonterminals and variables use CamelCase, as do random VariantNames not part of Rust syntax,
;   like TyAdt

(define-language patina-ty

  (; Tys = multiple Rust types
   Tys := (Ty ...))

  (; Ty = Rust type
   Ty :=
      TyNotVar
      VarId
      )
  (TyNotVar := (TyApply TyName Parameters))
  (TyName := AdtId
          TraitId
          VarId ; must be universally bound
          (TraitId AssociatedTyId)
          ScalarId
          (Ref MaybeMut)
          (Tuple number)
          )

  (; Lifetime = Lifetime of a Rust reference
   Lifetime :=
            static
            VarId)

  (; Env = environment for checking trait predicates and the like
   Env := (Universe Bindings Clauses Hypotheses))

  (Bindings := (Binding ...))
  (Binding := (VarId Quantifier Universe))
  (Quantifier := ForAll Exists)

  (; AdtRef = reference to a struct, enum, or union
   AdtRef := (AdtId Parameters))

  (; TraitRef = reference to a trait
   TraitRef := (TraitId Parameters))

  (; MaybeMut = either mut or not
   MaybeMut := () (mut) )

  (Substitution-e Error Substitution)
  (; Substitution = [A -> B] listing
   Substitution := ((VarId Parameter) ...))

  (KindedVarIds := (KindedVarId ...))
  (KindedVarId := (VarKind VarId))
  (VarKind := TyVar LifetimeVar)

  (; parameters -- values for generic parameters
   Parameters := (Parameter ...))

  (; parameter -- value for a generic parameter
   Parameter := Ty Lifetime)

  (ParameterPairs := (ParameterPair ...))
  (ParameterPair :=
                 (Ty Ty)
                 (Lifetime Lifetime))

  (; predicates are the "atomic" items that we can prove and so forth
   Predicate :=
             (Implemented TraitRef)
             (HasImpl TraitRef)
             (WellFormed ty)
             )

  ;
  (Goals = (Goal ...))
  (Goal :=
        Predicate
        (All Goals)
        (Any Goals)
        (Implies Hypotheses Goal)
        (ForAll KindedVarIds Goal)
        (Exists KindedVarIds Goal)
        )

  (Hypotheses = (Hypothesis ...))
  (Hypothesis :=
              Predicate
              (Implies predicate predicate)
              (ForAll kinded-var-ids hypothesis)
              )

  (Clauses := (Clause ...))
  (Clause :=
          Predicate
          (Implies Goals Predicate)
          (ForAll KindedVarIds Clause)
          )

  (Universe := (U number))

  ; ids
  (VarIds := (VarId ...))
  ((AdtId
    ScalarId
    VarId
    TraitId
    AssociatedTyId) variable-not-otherwise-mentioned)

  #:binding-forms
  (ForAll ((VarKind VarId) ...) any #:refers-to (shadow VarId ...))
  (Exists ((VarKind VarId) ...) any #:refers-to (shadow VarId ...))
  )

(define-term
  EmptyEnv
  ((U 0) () () ())
  )

(define-metafunction patina-ty
  env-hypotheses : Env -> Hypotheses

  [(env-hypotheses (Universe Bindings Clauses Hypotheses)) Hypotheses]
  )

(define-metafunction patina-ty
  env-clauses : Env -> Hypotheses

  [(env-clauses (Universe Bindings Clauses Hypotheses)) Clauses]
  )

(define-metafunction patina-ty
  env-bindings : Env -> Bindings

  [(env-bindings (Universe Bindings Clauses Hypotheses)) Bindings]
  )

(define-metafunction patina-ty
  env-with-binding : Env Binding ... -> Env

  [(env-with-binding Env Binding_0 ...)
   (Universe (Binding_0 ... Binding ...) Clauses Hypotheses)
   (where/error (Universe (Binding ...) Clauses Hypotheses) Env)
   ]
  )

(define-metafunction patina-ty
  env-universe : Env -> Universe

  [(env-universe (Universe Bindings Clauses Hypotheses)) Universe]
  )

(define-metafunction patina-ty
  env-with-universe : Env Universe -> Env

  [(env-with-universe Env Universe)
   (Universe Bindings Clauses Hypotheses)
   (where (_ Bindings Clauses Hypotheses) Env)
   ]

  )

(define-metafunction patina-ty
  binding-in-env : Env VarId -> (Quantifier Universe)

  [(binding-in-env Env VarId)
   (Quantifier Universe)
   (where/error (_ ... (VarId Quantifier Universe) _ ...) (env-bindings Env))]

  )


(define-metafunction patina-ty
  ; True if the given variable appears free in the given term.
  appears-free : VarId any -> boolean

  [(appears-free VarId any)
   ,(not (alpha-equivalent? patina-ty (term any) (term any_1)))
   (where/error any_1 (substitute any VarId (TyApply VarId ())))
   ]
  )

(define-metafunction patina-ty
  ; Returns the set of variables that appear free in the given term.
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
  )

(define-metafunction patina-ty
  not? : boolean -> boolean
  [(not? false) true]
  [(not? true) false]
  )

(define-metafunction patina-ty
  all? : (boolean ...) -> boolean
  [(all? (true ...)) true]
  [(all? _) false]
  )

(define-metafunction patina-ty
  any? : (boolean ...) -> boolean
  [(any? (_ ... true _ ...)) true]
  [(any? _) false]
  )

(define-metafunction patina-ty
  min-universe : Universe ... -> Universe
  [(min-universe Universe) Universe]
  [(min-universe (U number) ...)
   (U ,(apply min (term (number ...))))
   ])

(module+ test
  (test-match patina-ty
              Goal
              (term (All ())))
  (test-match patina-ty
              AdtId
              (term somevar))
  (test-match patina-ty
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
                     (env-with-binding EmptyEnv (x ForAll (U 22)))
                     x))
              (term (ForAll (U 22))))

  (test-equal (term (min-universe (U 3) (U 5) (U 2)))
              (term (U 2)))

  )