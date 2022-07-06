#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         )
(provide lower-to-decl/WhereClause
         )

(define-metafunction formality-rust
  lower-to-decl/WhereClause : Rust/WhereClause -> WhereClause

  [(lower-to-decl/WhereClause (for KindedVarIds Rust/WhereClause))
   (∀ KindedVarIds (lower-to-decl/WhereClause Rust/WhereClause))]

  [(lower-to-decl/WhereClause (UserTy : TraitId [UserParameter ...]))
   ((user-ty UserTy) : TraitId [(user-parameter UserParameter) ...])]

  [(lower-to-decl/WhereClause ((ParameterKind_1 UserParameter_1) : (ParameterKind_2 UserParameter_2)))
   ((ParameterKind_1 (user-parameter UserParameter_1)) : (ParameterKind_2 (user-parameter UserParameter_2)))]

  [(lower-to-decl/WhereClause (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2 == UserTy_v))
   ((user-ty (< UserTy_s as TraitId UserParameters_1 > :: AssociatedTyId UserParameters_2)) == (user-ty UserTy_v))]

  )