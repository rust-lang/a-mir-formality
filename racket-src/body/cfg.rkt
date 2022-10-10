#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "locations.rkt"
         )
(provide control-flow-graph
         control-flow-graph-from-Γ
         cfg-node-at
         cfg-edges
         cfg-locations
         )

(define-metafunction formality-body
  control-flow-graph-from-Γ : Γ -> Cfg

  [(control-flow-graph-from-Γ Γ)
   (control-flow-graph BasicBlockDecls)
   (where/error BasicBlockDecls (basic-block-decls-of-Γ Γ))
   ]

  )
(define-metafunction formality-body
  control-flow-graph : BasicBlockDecls -> Cfg

  [(control-flow-graph [BasicBlockDecl ...])
   ([LocatedCfgNode ... ...] [CfgEdge ... ...])
   (where/error [[LocatedCfgNode ...] ...] [(control-flow-graph-nodes-from-block BasicBlockDecl) ...])
   (where/error [[CfgEdge ...] ...] [(control-flow-graph-edges-from-block BasicBlockDecl) ...])
   ]

  )

(define-metafunction formality-body
  cfg-node-at : Cfg Location -> CfgNode

  [(cfg-node-at Cfg Location)
   CfgNode
   (where/error ([_ ... (Location CfgNode) _ ...] _) Cfg)
   ]

  )

(define-metafunction formality-body
  cfg-locations : Cfg -> Locations

  [(cfg-locations Cfg)
   [Location ...]
   (where/error ([(Location CfgNode) ...] _) Cfg)
   ]

  )

(define-metafunction formality-body
  cfg-edges : Cfg -> CfgEdges

  [(cfg-edges Cfg)
   CfgEdges
   (where/error (_ CfgEdges) Cfg)
   ]

  )

(define-metafunction formality-body
  control-flow-graph-nodes-from-block : BasicBlockDecl -> LocatedCfgNodes

  [(control-flow-graph-nodes-from-block BasicBlockDecl)
   [(Location_s Statement) ... (Location_t Terminator)]
   (where/error ([(Location_s Statement) ...] (Location_t Terminator)) (basic-block-locations BasicBlockDecl))
   ]

  )

(define-metafunction formality-body
  control-flow-graph-edges-from-block : BasicBlockDecl -> CfgEdges

  [(control-flow-graph-edges-from-block BasicBlockDecl)
   [; each statement goes to the statement after it
    (Location_0 Location_succ_0) (Location_1 Location_succ_1) ...
                                 ; and the terminator goes .. elsewhere
                                 CfgEdge_term ...]

   ; locations of each statement/terminator in the block
   (where ([(Location_0 _) (Location_1 _) ...] (Location_t Terminator)) (basic-block-locations BasicBlockDecl))

   ; shift locations by one to get successors for the statements, putting terminator at the end
   (where/error [Location_succ_0 Location_succ_1 ...] [Location_1 ... Location_t])
   (where/error [CfgEdge_term ...] (control-flow-graph-terminator-edges Location_t Terminator))
   ]

  [(control-flow-graph-edges-from-block BasicBlockDecl)
   (control-flow-graph-terminator-edges Location_t Terminator)
   (where ([] (Location_t Terminator)) (basic-block-locations BasicBlockDecl))
   ]

  )

(define-metafunction formality-body
  control-flow-graph-terminator-edges : Location_t Terminator -> CfgEdges

  [(control-flow-graph-terminator-edges Location_t (goto BasicBlockId))
   [(Location_t (BasicBlockId @ 0))]
   ]

  [(control-flow-graph-terminator-edges Location_t resume)
   []
   ]

  [(control-flow-graph-terminator-edges Location_t abort)
   []
   ]

  [(control-flow-graph-terminator-edges Location_t return)
   []
   ]

  [(control-flow-graph-terminator-edges Location_t unreachable)
   []
   ]

  [(control-flow-graph-terminator-edges Location_t (drop Place [BasicBlockId ...]))
   [(Location_t (BasicBlockId @ 0)) ...]
   ]

  [(control-flow-graph-terminator-edges Location_t (drop-and-replace Place Operand [BasicBlockId ...]))
   [(Location_t (BasicBlockId @ 0)) ...]
   ]

  [(control-flow-graph-terminator-edges Location_t (call Operand Operands Place [BasicBlockId ...]))
   [(Location_t (BasicBlockId @ 0)) ...]
   ]

  [(control-flow-graph-terminator-edges Location_t (assert Operand boolean [BasicBlockId ...]))
   [(Location_t (BasicBlockId @ 0)) ...]
   ]

  [(control-flow-graph-terminator-edges Location_t (switch-int Operand Ty [(number BasicBlockId_n) ...] [BasicBlockId_o ...]))
   [(Location_t (BasicBlockId @ 0)) ...]
   (where/error [BasicBlockId ...] [BasicBlockId_n ... BasicBlockId_o ...])
   ]

  )

(module+ test
  (redex-let*
   formality-body

   [(BasicBlockDecls (term [(bb0 { [(_0 = (use (copy _1))) ; _0 = _1
                                    ]
                                   return
                                   })
                            ]
                           ))]
   (test-equal (term (control-flow-graph BasicBlockDecls))
               (term ((((bb0 @ 0) (_0 = (use (copy _1))))
                       ((bb0 @ 1) return))
                      (((bb0 @ 0) (bb0 @ 1))))))

   )
  )