#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide debone-predicate
         )

(define-metafunction formality-ty
  debone-predicate : Predicate -> Predicate/Deboned

  [(debone-predicate (is-implemented (TraitId Parameters)))
   ((is-implemented TraitId) Parameters)
   ]

  [(debone-predicate (has-impl (TraitId Parameters)))
   ((has-impl TraitId) Parameters)
   ]

  [(debone-predicate (well-formed (ParameterKind Parameter)))
   ((well-formed ParameterKind) (Parameter))
   ]

  [(debone-predicate (normalizes-to (alias-ty AliasName (Parameter ...)) Ty))
   ((normalizes-to AliasName) (Parameter ... Ty))
   ]

  )