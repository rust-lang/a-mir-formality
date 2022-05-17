#lang racket
(require redex "grammar.rkt")
(provide (all-defined-out))

(define-metafunction formality-mir
  apply-projections : Place Projections -> Place

  [(apply-projections Place ()) Place]

  [(apply-projections Place (* Projection ...))
   (apply-projections (* Place) (Projection ...))]

  [(apply-projections Place ((field FieldId) Projection ...))
   (apply-projections (field Place FieldId) (Projection ...))]

  [(apply-projections Place ((index LocalId) Projection ...))
   (apply-projections (index Place LocalId) (Projection ...))]

  [(apply-projections Place ((downcast VariantId) Projection ...))
   (apply-projections (downcast Place VariantId) (Projection ...))]
  )

(define-metafunction formality-mir
  unpack-projections : Place -> (LocalId Projections)

  [(unpack-projections LocalId) (LocalId ())]

  [(unpack-projections (* Place))
   (LocalId (Projection ... *))
   (where (LocalId (Projection ...)) (unpack-projections Place))]

  [(unpack-projections (field Place FieldId))
   (LocalId (Projection ... (field FieldId)))
   (where (LocalId (Projection ...)) (unpack-projections Place))]

  [(unpack-projections (index Place LocalId))
   (LocalId (Projection ... (index LocalId)))
   (where (LocalId (Projection ...)) (unpack-projections Place))]

  [(unpack-projections (downcast Place VariantId))
   (LocalId (Projection ... (downcast VariantId)))
   (where (LocalId (Projection ...)) (unpack-projections Place))]
  )
