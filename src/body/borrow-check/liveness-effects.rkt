#lang racket
(require racket/set
         redex/reduction-semantics
         "../grammar.rkt"
         )
(provide union-local-ids
         minus-local-ids
         chain-liveness-effects
         )

(define-metafunction formality-body
  ;; Given the effects of multiple statements (or terms, or whatever),
  ;; returns the combined effects of executing them all in sequence.
  chain-liveness-effects : LivenessEffects ... -> LivenessEffects

  [(chain-liveness-effects)
   (reads: [] drops: [] writes: [])
   ]

  [(chain-liveness-effects LivenessEffects_0 LivenessEffects_1 ...)
   (reads: LocalIds_r drops: LocalIds_d writes: LocalIds_w)

   ; Effects E0 of the first statement
   (where/error (reads: LocalIds_r0 drops: LocalIds_d0 writes: LocalIds_w0) LivenessEffects_0)

   ; Combined effects E1 of the remaining statements
   (where/error (reads: LocalIds_r1 drops: LocalIds_d1 writes: LocalIds_w1) (chain-liveness-effects LivenessEffects_1 ...))

   ; Anything written by term 0 is written
   ; Anything written by term 1 is written, as long as it is not read by term 0
   (where/error LocalIds_w (union-local-ids LocalIds_w0
                                            (minus-local-ids LocalIds_w1 LocalIds_r0 LocalIds_d0)))

   ; Anything read by term 0 is read
   ; Anything read by term 1 is read, as long as it is not written by term 0
   (where/error LocalIds_r (union-local-ids LocalIds_r0
                                            (minus-local-ids LocalIds_r1 LocalIds_w0)))

   ; Anything dropped by term 0 is dropped
   ; Anything dropped by term 1 is dropped, as long as it is not read or written by term 0
   (where/error LocalIds_d (union-local-ids LocalIds_d0
                                            (minus-local-ids LocalIds_d1 LocalIds_r0 LocalIds_w0)))
   ]
  )

(define-metafunction formality-body
  union-local-ids : LocalIds ... -> LocalIds

  [(union-local-ids)
   []
   ]

  [(union-local-ids LocalIds ...)
   ,(apply set-union (term [LocalIds ...]))
   ]
  )

(define-metafunction formality-body
  minus-local-ids : LocalIds_set LocalIds_del ... -> LocalIds

  [(minus-local-ids LocalIds_set LocalIds_del ...)
   ,(apply set-subtract (term [LocalIds_set LocalIds_del ...]))
   ]
  )
