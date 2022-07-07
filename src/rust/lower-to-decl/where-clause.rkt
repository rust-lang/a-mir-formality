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
   (∀ KindedVarIds (lower-to-decl/WhereClause Rust/WhereClause))]

  [(lower-to-decl/WhereClause (UserTy : TraitId [UserParameter ...]))
   (is-implemented (TraitId [(user-ty UserTy) (user-parameter UserParameter) ...]))]

  [(lower-to-decl/WhereClause (UserParameter_1 : UserParameter_2))
   ((user-parameter UserParameter_1) -outlives- (user-parameter UserParameter_2))]

  [(lower-to-decl/WhereClause (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2 == UserTy_v))
   (normalizes-to (user-ty (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2))
                  (user-ty UserTy_v))]

  )
