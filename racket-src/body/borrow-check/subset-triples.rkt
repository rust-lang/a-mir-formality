#lang racket
(require racket/set
         redex/reduction-semantics
         "../grammar.rkt"
         )
(provide
 )

(define-metafunction formality-body
  ;; Compute live variables on entry to each basic block.
  liveness : BasicBlockDecls -> LiveVariablesBeforeBlocks

  [(liveness BasicBlockDecls)
   (liveness-fix BasicBlockDecls LiveVariablesBeforeBlocks)

   (where/error [(BasicBlockId _) ...] BasicBlockDecls)
   (where/error LiveVariablesBeforeBlocks [(BasicBlockId (reads: [] drops: [])) ...])
   ]
  )