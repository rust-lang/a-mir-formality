#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         )
(provide lower-to-decl/WhereClause
         )

(define-metafunction formality-rust
  lower-to-decl/WhereClause : Rust/WhereClause -> Biformula

  [(lower-to-decl/WhereClause (for KindedVarIds Rust/WhereClause))
   (∀ KindedVarIds
      (implies [Biformula_req ...]
               (lower-to-decl/WhereClause Rust/WhereClause)))
   (where/error [Biformula_req ...] (well-formed-requirements Rust/WhereClause))
   ]

  [(lower-to-decl/WhereClause (UserTy : TraitId [UserParameter ...]))
   (is-implemented (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...]))]

  [(lower-to-decl/WhereClause (UserParameter_1 : UserParameter_2))
   ((user-parameter UserParameter_1) -outlives- (user-parameter UserParameter_2))]

  [(lower-to-decl/WhereClause (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2 == UserTy_v))
   (normalizes-to (user-ty (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2))
                  (user-ty UserTy_v))]

  )

(define-metafunction formality-rust
  well-formed-requirements : Rust/WhereClause -> Biformulas

  [(well-formed-requirements (for KindedVarIds Rust/WhereClause))
   []
   ]

  [(well-formed-requirements (UserTy : TraitId [UserParameter ...]))
   [(well-formed-where-clause (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...]))]
   ]

  [(well-formed-requirements (UserParameter_1 : UserParameter_2))
   [; FIXME -- we need the KindedVarIds to classify userparameter as types or lifetimes.
    ;
    ; Maybe  even better if we modify the grammar to add the ParameterKind back in again?
    ]
   ]

  [(well-formed-requirements (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2 == UserTy_v))
   [(well-formed (type (user-ty (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2))))
    (well-formed (type (user-ty UserTy_v)))
    ]
   ]

  )
