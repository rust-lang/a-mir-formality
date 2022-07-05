#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "where-clause.rkt"
         )
(provide lower-to-decl/BoundsClause
         )

(define-metafunction formality-rust
  lower-to-decl/BoundsClause : Rust/BoundsClause -> BoundsClause

  [(lower-to-decl/BoundsClause [Rust/Bound ...])
   (: (type VarId_fresh) [(lower-to-decl/WhereClause Rust/WhereClause) ...])
   ; we have to introduce a fresh variable VarId to become the bound variable that is present
   ; in decl syntax, but not in rust
   (where/error VarId_fresh ,(variable-not-in (term [Rust/Bound ...]) 'X))
   (where/error [Rust/WhereClause ...] [(instantiate-rust-bound VarId_fresh Rust/Bound) ...])]
  )

(define-metafunction formality-rust
  ;; Convert a Rust/Bound, applied to a type named VarId (which does not appear
  ;; in Rust/Bound) to a Rust/WhereClause.
  instantiate-rust-bound : VarId Rust/Bound -> Rust/WhereClause

  [(instantiate-rust-bound VarId (for KindedVarIds Rust/Bound))
   (for KindedVarIds (instantiate-rust-bound VarId Rust/Bound))
   ]

  [(instantiate-rust-bound VarId (TraitId UserParameters))
   (VarId : TraitId UserParameters)
   ]

  [(instantiate-rust-bound VarId (TraitId UserParameters_t :: AssociatedTyId UserParameters_i == Rust/Ty))
   (< VarId as TraitId UserParameters_t > :: AssociatedTyId UserParameters_i == Rust/Ty)
   ]

  [(instantiate-rust-bound VarId KindedUserParameter)
   ((type VarId) : KindedUserParameter)
   ]

  )