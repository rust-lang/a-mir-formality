#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "parameter-kind.rkt"
         )
(provide lower-to-decl/WhereClause
         )

(define-metafunction formality-rust
  ;; Lower a `Rust/WhereClause` to a logical Biformula.
  ;;
  ;; * `KindedVarIds` -- set of in-scope variables, used to determine whether a UserParameter is a type or lifetime
  lower-to-decl/WhereClause : KindedVarIds Rust/WhereClause -> Biformula

  [(lower-to-decl/WhereClause KindedVarIds_cx (for KindedVarIds Rust/WhereClause))
   (∀ KindedVarIds
      (implies [Biformula_req ...]
               (lower-to-decl/WhereClause KindedVarIds_all Rust/WhereClause)))
   (where/error KindedVarIds_all (flatten [KindedVarIds_cx KindedVarIds]))
   (where/error [Biformula_req ...] (well-formed-requirements KindedVarIds_all Rust/WhereClause))
   ]

  [(lower-to-decl/WhereClause _ (UserTy : TraitId [UserParameter ...]))
   (is-implemented (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...]))]

  [(lower-to-decl/WhereClause _ (UserParameter_1 : UserParameter_2))
   ((user-parameter UserParameter_1) -outlives- (user-parameter UserParameter_2))]

  [(lower-to-decl/WhereClause _ (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2 == UserTy_v))
   (normalizes-to (user-ty (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2))
                  (user-ty UserTy_v))]

  )

(define-metafunction formality-rust
  well-formed-requirements : KindedVarIds Rust/WhereClause -> Biformulas

  [(well-formed-requirements KindedVarIds (for KindedVarIds Rust/WhereClause))
   []
   ]

  [(well-formed-requirements KindedVarIds (UserTy : TraitId [UserParameter ...]))
   [(well-formed-where-clause (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...]))]
   ]

  [(well-formed-requirements KindedVarIds (UserParameter_1 : UserParameter_2))
   [(well-formed (ParameterKind_1 (user-parameter UserParameter_1)))
    (well-formed (ParameterKind_2 (user-parameter UserParameter_2)))
    ]
   (where/error ParameterKind_1 (parameter-kind-of-user-parameter KindedVarIds UserParameter_1))
   (where/error ParameterKind_2 (parameter-kind-of-user-parameter KindedVarIds UserParameter_1))
   ]

  [(well-formed-requirements KindedVarIds (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2 == UserTy_v))
   [(well-formed (type (user-ty (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2))))
    (well-formed (type (user-ty UserTy_v)))
    ]
   ]

  )
