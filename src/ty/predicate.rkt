#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide ty:debone-predicate
         )

(define-metafunction formality-ty
  ;; Definition of the debone-predicate hook from the logic layer
  ;; for the predicates defined in this layer.
  ty:debone-predicate : Predicate -> Predicate/Deboned

  [(ty:debone-predicate (is-implemented (TraitId Parameters)))
   ((is-implemented TraitId) Parameters -> ())
   ]

  [(ty:debone-predicate (has-impl (TraitId Parameters)))
   ((has-impl TraitId) Parameters -> ())
   ]

  [(ty:debone-predicate (well-formed-trait-ref (TraitId Parameters)))
   ((well-formed-trait-ref TraitId) Parameters -> ())
   ]

  [(ty:debone-predicate (well-formed (ParameterKind Parameter)))
   ((well-formed ParameterKind) (Parameter) -> ())
   ]

  [(ty:debone-predicate (well-formed-adt (rigid-ty AdtId Parameters)))
   ((well-formed-adt AdtId) Parameters -> ())
   ]

  [(ty:debone-predicate (well-formed-alias (alias-ty AliasName Parameters)))
   ((well-formed-alias AliasName) Parameters -> ())
   ]

  [(ty:debone-predicate (normalizes-to (alias-ty AliasName (Parameter ...)) Ty))
   ((normalizes-to AliasName) (Parameter ...) -> (Ty))
   ]

  )