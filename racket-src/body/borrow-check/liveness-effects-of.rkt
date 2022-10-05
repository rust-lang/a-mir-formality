#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         "liveness-effects.rkt"
         )
(provide liveness-effects-of-terminator
         liveness-effects-of-evaluating-statements
         liveness-effects-of-evaluating-statement
         )

;; Methods for computing the liveness effects of individual MIR terms.

(define-metafunction formality-body
  liveness-effects-of-terminator : Terminator -> [TerminatorLivenessEffects ...]

  [(liveness-effects-of-terminator (goto BasicBlockId))
   (BasicBlockId (reads: [] drops: [] writes: []))
   ]

  [(liveness-effects-of-terminator resume)
   []
   ]

  [(liveness-effects-of-terminator abort)
   []
   ]

  [(liveness-effects-of-terminator return)
   []
   ]

  [(liveness-effects-of-terminator unreachable)
   []
   ]

  [(liveness-effects-of-terminator (drop Place [BasicBlockId ...]))
   [(BasicBlockId (liveness-effects-of-dropping-place Place)) ...]
   ]

  [(liveness-effects-of-terminator (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId_ok]))
   [(BasicBlockId_ok (chain-liveness-effects (liveness-effects-of-evaluating-operand Operand_fn)
                                             (liveness-effects-of-evaluating-operand Operand_arg) ...
                                             (liveness-effects-of-writing-place Place_dest)))
    ]
   ]

  [(liveness-effects-of-terminator (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId_ok BasicBlockId_panic]))
   [(BasicBlockId_ok (chain-liveness-effects (liveness-effects-of-evaluating-operand Operand_fn)
                                             (liveness-effects-of-evaluating-operand Operand_arg) ...
                                             (liveness-effects-of-writing-place Place_dest)))

    (BasicBlockId_panic (chain-liveness-effects (liveness-effects-of-evaluating-operand Operand_fn)
                                                (liveness-effects-of-evaluating-operand Operand_arg) ...))
    ]
   ]

  )

(define-metafunction formality-body
  liveness-effects-of-evaluating-statements : Statements -> LivenessEffects

  [(liveness-effects-of-evaluating-statements [])
   (reads: [] drops: [] writes: [])
   ]

  [(liveness-effects-of-evaluating-statements [Statement_0 Statement_1 ...])
   (chain-liveness-effects LivenessEffects_0 LivenessEffects_1)
   (where/error LivenessEffects_1 (liveness-effects-of-evaluating-statements [Statement_1 ...]))
   (where/error LivenessEffects_0 (liveness-effects-of-evaluating-statement Statement_0))
   ]
  )

(define-metafunction formality-body
  liveness-effects-of-evaluating-statement : Statement -> LivenessEffects

  [(liveness-effects-of-evaluating-statement (Place = Rvalue))
   (chain-liveness-effects (liveness-effects-of-evaluating-rvalue Rvalue)
                           (liveness-effects-of-writing-place Place))
   ]

  [(liveness-effects-of-evaluating-statement (set-discriminant Place VariantId))
   (liveness-effects-of-reading-place Place)
   ]

  [(liveness-effects-of-evaluating-statement (storage-live LocalId))
   (reads: [] drops: [] writes: [])
   ]

  [(liveness-effects-of-evaluating-statement (storage-dead LocalId))
   (reads: [] drops: [] writes: [])
   ]

  [(liveness-effects-of-evaluating-statement noop)
   (reads: [] drops: [] writes: [])
   ]

  [(liveness-effects-of-evaluating-statement (fake-read Place))
   (liveness-effects-of-reading-place Place)
   ]

  )

(define-metafunction formality-body
  liveness-effects-of-evaluating-rvalue : Rvalue -> LivenessEffects

  [(liveness-effects-of-evaluating-rvalue (use Operand))
   (liveness-effects-of-evaluating-operand Operand)
   ]

  [(liveness-effects-of-evaluating-rvalue (repeat Operand Constant))
   (liveness-effects-of-evaluating-operand Operand)
   ]

  [(liveness-effects-of-evaluating-rvalue (ref Lt MaybeMut Place))
   (liveness-effects-of-reading-place Place)
   ]

  [(liveness-effects-of-evaluating-rvalue (addr-of MaybeMut Place))
   (liveness-effects-of-reading-place Place)
   ]

  [(liveness-effects-of-evaluating-rvalue (len Place))
   (liveness-effects-of-reading-place Place)
   ]

  [(liveness-effects-of-evaluating-rvalue (BinaryOp Operand_l Operand_r))
   (chain-liveness-effects (liveness-effects-of-evaluating-operand Operand_l)
                           (liveness-effects-of-evaluating-operand Operand_r))
   ]

  )

(define-metafunction formality-body
  liveness-effects-of-evaluating-operand : Operand -> LivenessEffects

  [(liveness-effects-of-evaluating-operand (CopyMove Place))
   (liveness-effects-of-reading-place Place)
   ]

  [(liveness-effects-of-evaluating-operand (const _))
   (reads: [] drops: [] writes: [])
   ]
  )

(define-metafunction formality-body
  liveness-effects-of-reading-place : Place -> LivenessEffects

  [(liveness-effects-of-reading-place LocalId)
   (reads: [LocalId] drops: [] writes: [])
   ]

  [(liveness-effects-of-reading-place (* Place))
   (liveness-effects-of-reading-place Place)
   ]

  [(liveness-effects-of-reading-place (field Place FieldId))
   (liveness-effects-of-reading-place Place)
   ]

  [(liveness-effects-of-reading-place (index Place LocalId_index))
   (chain-liveness-effects (liveness-effects-of-reading-place Place)
                           (reads: [LocalId_index] drops: [] writes: []))
   ]

  [(liveness-effects-of-reading-place (downcast Place VariantId))
   (liveness-effects-of-reading-place Place)
   ]
  )

(define-metafunction formality-body
  liveness-effects-of-dropping-place : Place -> LivenessEffects

  [(liveness-effects-of-dropping-place LocalId)
   (reads: [] drops: [LocalId] writes: [])
   ]

  [(liveness-effects-of-dropping-place (* Place))
   (liveness-effects-of-dropping-place Place)
   ]

  [(liveness-effects-of-dropping-place (field Place FieldId))
   (liveness-effects-of-dropping-place Place)
   ]

  [(liveness-effects-of-dropping-place (index Place LocalId_index))
   (chain-liveness-effects (liveness-effects-of-dropping-place Place)
                           (reads: [LocalId_index] drops: [] writes: []))
   ]

  [(liveness-effects-of-dropping-place (downcast Place VariantId))
   (liveness-effects-of-dropping-place Place)
   ]
  )

(define-metafunction formality-body
  liveness-effects-of-writing-place : Place -> LivenessEffects

  [(liveness-effects-of-writing-place LocalId)
   (reads: [] drops: [] writes: [LocalId])
   ]

  [(liveness-effects-of-writing-place Place)
   (liveness-effects-of-reading-place Place)
   ]
  )