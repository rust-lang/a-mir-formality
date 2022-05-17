#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide debone-predicate
         )

(define-metafunction formality-ty
  debone-predicate : Predicate -> Predicate/Deboned

  [(debone-predicate (Implemented (TraitId Parameters)))
   ((Implemented TraitId) Parameters)
   ]

  [(debone-predicate (has-impl (TraitId Parameters)))
   ((has-impl TraitId) Parameters)
   ]

  [(debone-predicate (WellFormed (ParameterKind Parameter)))
   ((WellFormed ParameterKind) (Parameter))
   ]

  [(debone-predicate (Normalize (TyAlias AliasName (Parameter ...)) Ty))
   ((Normalize AliasName) (Parameter ... Ty))
   ]

  )