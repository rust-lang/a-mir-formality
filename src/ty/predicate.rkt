#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide flay-predicate
         )

(define-metafunction formality-ty
  flay-predicate : Predicate -> Predicate/Flayed

  [(flay-predicate (Implemented (TraitId Parameters)))
   ((Implemented TraitId) Parameters)
   ]

  [(flay-predicate (HasImpl (TraitId Parameters)))
   ((HasImpl TraitId) Parameters)
   ]

  [(flay-predicate (WellFormed (ParameterKind Parameter)))
   ((WellFormed ParameterKind) (Parameter))
   ]

  [(flay-predicate (Normalize (TyAlias AliasName (Parameter ...)) Ty))
   ((Normalize AliasName) (Parameter ... Ty))
   ]

  )