#lang racket
(require redex "grammar.rkt")
(provide (all-defined-out))

(define-metafunction formality-mir
  apply-projection : Place Projection -> Place

  [(apply-projection Place *) (* Place)]
  [(apply-projection Place (field FieldId)) (field Place FieldId)]
  [(apply-projection Place (index LocalId)) (index Place LocalId)]
  [(apply-projection Place (downcast VariantId)) (downcast Place VariantId)]
  )

(define-metafunction formality-mir
  apply-projections : Place Projections -> Place

  [(apply-projections Place ()) Place]

  [(apply-projections Place (Projection_hd Projection_tl ...))
   (apply-projections (apply-projection Place Projection_hd) (Projection_tl ...))]
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
