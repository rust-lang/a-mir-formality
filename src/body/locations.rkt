#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide basic-block-locations)

(define-metafunction formality-body
  basic-block-locations : BasicBlockDecl -> (StatementAtLocations TerminatorAtLocation)

  [(basic-block-locations (BasicBlockId ((Statement ...) Terminator)))
   (StatementAtLocations TerminatorAtLocation)
   (where/error (number_s ...) ,(stream->list (in-range (length (term (Statement ...))))))
   (where/error number_t ,(length (term (Statement ...))))
   (where/error StatementAtLocations (((BasicBlockId @ number_s) Statement) ...))
   (where/error TerminatorAtLocation ((BasicBlockId @ number_t) Terminator))
   ]
  )

(module+ test

  (redex-let*
   formality-body

   [(BasicBlockDecl (term (bb0 (((storage-live l0)
                                 noop
                                 (storage-dead l0))
                                (goto bb1)))))]

   (test-equal (term (basic-block-locations BasicBlockDecl))
               (term ((((bb0 @ 0) (storage-live l0))
                       ((bb0 @ 1) noop)
                       ((bb0 @ 2) (storage-dead l0)))
                      ((bb0 @ 3) (goto bb1))
                      ))
               )

   )

  )