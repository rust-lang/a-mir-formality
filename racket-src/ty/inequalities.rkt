#lang racket
(require redex/reduction-semantics
         "../logic/env.rkt"
         "grammar.rkt"
         )
(provide known-bounds
         invert-inequality-op
         )

(define-metafunction formality-ty
  ;; Returns the bounds on `VarId_in` found in the environment, or `(() ())` if none are found.
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  invert-inequality-op : InequalityOp -> InequalityOp

  [(invert-inequality-op <=) >=]
  [(invert-inequality-op >=) <=]
  [(invert-inequality-op -outlives-) -outlived-by-]
  [(invert-inequality-op -outlived-by-) -outlives-]
  )

(define-metafunction formality-ty
  ;; Given a variable `VarId_in` and a inequality op `op` (e.g. `<=`), returns the known
  ;; bounds `P` such that `P (op) VarId` (e.g., `P <= VarId`).
  ;;
  ;; (Note that these are stored in the environment with `VarId` listed first, so we actually
  ;; look for inequalities like `VarId >= P`.)
  known-bounds : Env_in InequalityOp VarId_in -> Parameters_ub
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(known-bounds Env InequalityOp_◃ VarId)
   Parameters
   (; we store bounds like `VarId <= X`, so if we want `X <= VarId`, we have to
    ; search for `VarId >= X`.
    where/error InequalityOp_▹ (invert-inequality-op InequalityOp_◃))
   (where (_ ... (VarId InequalityOp_▹ Parameters) _ ...) (env-inequalities Env))
   ]

  [(known-bounds Env InequalityOp VarId)
   ()
   ]

  )
