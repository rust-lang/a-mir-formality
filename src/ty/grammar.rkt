#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))

(define-language patina-ty
  (tys := (ty ...))
  (; ty = grammar for Rust types
   ty :=
      (ty-adt adt-ref)
      (ty-dyn trait-ref)
      (ty-associated-ty associated-ty-id parameters)
      (ty-scalar scalar-id)
      (ty-ref maybe-mut lifetime ty)
      (ty-tuple (ty ...))
      (ty-var var-id)
      )

  (; a lifetime (!)
   lifetime :=
            lifetime-static
            (lifetime-var var-id))

  (; env = environment for checking trait predicates
   env := (program-clauses hypotheses))

  (; adt-ref = reference to a struct, enum, or union
   adt-ref := (adt-id parameters))

  (; trait-ref = reference to a trait
   trait-ref := (trait-id parameters))

  (; maybe-mut = either mut or not
   maybe-mut := () (mut) )

  (substitution := ((var-id parameter) ...))

  (kinded-var-ids := (kinded-var-id ...))
  (kinded-var-id := (var-kind var-id))
  (var-kind := ty-var lifetime-var)

  (; parameters -- values for generic parameters
   parameters := (parameter ...))

  (; parameter -- value for a generic parameter
   parameter := ty lifetime)

  (; predicates are the "atomic" items that we can prove and so forth
   predicate :=
             (implemented trait-ref)
             (has-impl trait-ref)
             (well-formed ty)
             )

  ;
  (goals = (goal ...))
  (goal :=
        predicate
        (all goals)
        (any goals)
        (implies hypotheses goal)
        (forall kinded-var-ids goal)
        (exists kinded-var-ids goal)
        )

  (hypotheses = (hypothesis ...))
  (hypothesis :=
              predicate
              (implies predicate predicate)
              (forall kinded-var-ids hypothesis)
              )

  (program-clauses := (program-clause ...))
  (program-clause :=
                  predicate
                  (implies goals predicate)
                  (forall kinded-var-ids program-clause)
                  )

  ; ids
  (var-ids := (var-id ...))
  ((adt-id
    scalar-id
    var-id
    trait-id
    associated-ty-id) identifier-not-otherwise-mentioned)

  #:binding-forms
  (forall ((var-kind var-id) ...) any #:refers-to (shadow var-id ...))
  (exists ((var-kind var-id) ...) any #:refers-to (shadow var-id ...))
  )
