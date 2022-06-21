#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "predicate.rkt"
         )
(provide user-ty
         )

(define-metafunction formality-ty
  user-ty : UserTy -> Ty

  [(user-ty ())
   (rigid-ty (tuple 0) ())
   ]

  [(user-ty (tuple UserTy ...))
   (rigid-ty (tuple number_arity) ((user-ty UserTy) ...))
   (where/error number_arity ,(length (term (UserTy ...))))
   ]

  [(user-ty (& Lt UserTy))
   (rigid-ty (ref ()) (Lt (user-ty UserTy)))
   ]

  [(user-ty (&mut Lt UserTy))
   (rigid-ty (ref mut) (Lt (user-ty UserTy)))
   ]

  [(user-ty ScalarId)
   (rigid-ty ScalarId ())
   ]

  [(user-ty (AdtId UserParameter ...))
   (rigid-ty AdtId ((user-parameter UserParameter) ...))
   ]

  [(user-ty (< UserTy as TraitId (UserParameter_trait ...) > :: AssociatedTyId (UserParameter_ty ...)))
   (alias-ty (TraitId AssociatedTyId) ((user-ty UserTy)
                                       (user-parameter UserParameter_trait) ...
                                       (user-parameter UserParameter_ty) ...))
   ]

  [(user-ty (fn (UserTy_arg ...) -> UserTy_ret))
   (rigid-ty (fn-ptr "Rust" number_args) ((user-ty UserTy_arg) ... (user-ty UserTy_ret)))
   (where/error number_args ,(length (term (UserTy_arg ...))))
   ]

  [(user-ty VarId) VarId]

  [(user-ty (for KindedVarIds UserTy))
   (∀ KindedVarIds (user-ty UserTy))
   ]
  )

(define-metafunction formality-ty
  user-parameter : UserParameter -> Parameter

  [(user-parameter Lt) Lt]

  [(user-parameter UserTy) (user-ty UserTy)]
  )