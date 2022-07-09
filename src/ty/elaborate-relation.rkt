#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )

(provide ty:elaborate-relation
         )

(define-metafunction formality-ty
  ty:elaborate-relation : Env Relation -> Hypotheses

  [(ty:elaborate-relation Env (Parameter_1 == Parameter_2))
   []
   ]

  [(ty:elaborate-relation Env (Parameter_1 SubtypeOp Parameter_2))
   []
   ]

  [; Rigid types outlive X only if all parts outlive X.
   ;
   ; e.g. `Vec<T>: 'a` implies `T: 'a`.
   (ty:elaborate-relation Env ((rigid-ty RigidName (Parameter ...)) OutlivesOp Parameter_2))
   [(Parameter OutlivesOp Parameter_2) ...]
   ]

  [(ty:elaborate-relation Env (Parameter_1 OutlivesOp Parameter_2))
   [] ; don't elaborate non-rigid types for now
   ]
  )

