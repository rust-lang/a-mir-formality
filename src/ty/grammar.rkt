#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

; Naming convention:
;
; * Rust keywords use the Rust keyword
; * Nonterminals and variables use CamelCase, as do random VariantNames not part of Rust syntax,
;   like TyAdt

(define-language patina-ty
  ; ty = Rust type
  (Tys := (Ty ...))
  (Ty :=
      (TyAdt AdtRef)
      (TyDyn TraitRef)
      (TyAssociatedTy AssociatedTyId Parameters)
      (TyScalar ScalarId)
      (TyRef MaybeMut Lifetime Ty)
      (TyTuple (Ty ...))
      VarId
      )

  ; a lifetime (!)
  (Lifetime :=
            static
            VarId)

  (; env = environment for checking trait predicates
   Env := (ProgramClauses Hypotheses))

  (; adt-ref = reference to a struct, enum, or union
   AdtRef := (AdtId Parameters))

  (; trait-ref = reference to a trait
   TraitRef := (TraitId Parameters))

  (; maybe-mut = either mut or not
   MaybeMut := () (mut) )

  (Substitution := ((VarId Parameter) ...))

  (KindedVarIds := (KindedVarId ...))
  (KindedVarId := (VarKind VarId))
  (VarKind := TyVar LifetimeVar)

  (; parameters -- values for generic parameters
   Parameters := (Parameter ...))

  (; parameter -- value for a generic parameter
   Parameter := Ty Lifetime)

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

  (ProgramClauses := (ProgramClause ...))
  (ProgramClause :=
                 Predicate
                 (Implies Goals Predicate)
                 (ForAll KindedVarIds ProgramClause)
                 )

  ; ids
  (VarIds := (VarId ...))
  (AdtId identifier-not-otherwise-mentioned)
  (ScalarId identifier-not-otherwise-mentioned)
  (VarId identifier-not-otherwise-mentioned)
  (TraitId identifier-not-otherwise-mentioned)
  (AssociatedTyId identifier-not-otherwise-mentioned)

  #:binding-forms
  (ForAll ((VarKind VarId) ...) any #:refers-to (shadow VarId ...))
  (Exists ((VarKind VarId) ...) any #:refers-to (shadow VarId ...))
  )

(module+ test
  (test-match patina-ty
              Goal
              (term (All ())))
  (test-match patina-ty
              AdtId
              (term somevar))
  )