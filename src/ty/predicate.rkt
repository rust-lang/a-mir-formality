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

  [(debone-predicate (well-formed (ParameterKind Parameter)))
   ((well-formed ParameterKind) (Parameter))
   ]

  [(debone-predicate (normalizes-to (TyAlias AliasName (Parameter ...)) Ty))
   ((normalizes-to AliasName) (Parameter ... Ty))
   ]

  )