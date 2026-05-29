use crate::grammar::Fallible;
use formality_core::Upcast;
use libspecr::hidden::GcCow;
use libspecr::list;
use libspecr::prelude::*;
use minirust_rs::lang;
use minirust_rs::mem::PtrType;

use super::minirust::*;
use super::scope::CodegenFn;

// ANCHOR: SemeRegion
/// A single-entry, multi-exit control flow region.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum SemeRegion {
    /// Anonymous open block being constructed. Has no name yet — a name is
    /// allocated lazily when the block is terminated or needs to be referenced.
    Block { stmts: Vec<lang::Statement> },

    /// Finalized blocks plus one open fallthrough block.
    Open {
        blocks: Map<lang::BbName, lang::BasicBlock>,
        entry: lang::BbName,
        fallthrough: lang::BbName,
        fallthrough_stmts: Vec<lang::Statement>,
    },

    /// Finalized blocks only. No successors.
    Closed {
        blocks: Map<lang::BbName, lang::BasicBlock>,
        entry: lang::BbName,
    },
}
// ANCHOR_END: SemeRegion

fn merge_blocks(
    target: &mut Map<lang::BbName, lang::BasicBlock>,
    source: Map<lang::BbName, lang::BasicBlock>,
) {
    for (k, v) in source {
        target.insert(k, v);
    }
}

impl SemeRegion {
    pub fn new() -> Self {
        SemeRegion::Block { stmts: Vec::new() }
    }

    /// Create a named region — already in Open state with the given bb as
    /// both entry and fallthrough.
    pub fn named(bb: lang::BbName) -> Self {
        SemeRegion::Open {
            blocks: Map::new(),
            entry: bb,
            fallthrough: bb,
            fallthrough_stmts: Vec::new(),
        }
    }

    pub fn with_stmt(self, stmt: lang::Statement) -> Self {
        match self {
            SemeRegion::Block { mut stmts } => {
                stmts.push(stmt);
                SemeRegion::Block { stmts }
            }
            SemeRegion::Open {
                blocks,
                entry,
                fallthrough,
                mut fallthrough_stmts,
            } => {
                fallthrough_stmts.push(stmt);
                SemeRegion::Open {
                    blocks,
                    entry,
                    fallthrough,
                    fallthrough_stmts,
                }
            }
            closed @ SemeRegion::Closed { .. } => closed,
        }
    }

    pub fn has_fallthrough(&self) -> bool {
        matches!(self, SemeRegion::Block { .. } | SemeRegion::Open { .. })
    }

    pub fn entry(&self) -> lang::BbName {
        match self {
            SemeRegion::Block { .. } => panic!("entry() called on anonymous Block"),
            SemeRegion::Open { entry, .. } => *entry,
            SemeRegion::Closed { entry, .. } => *entry,
        }
    }

    /// Finalize into basic blocks. Panics if called on an anonymous Block.
    pub fn into_blocks(self) -> Map<lang::BbName, lang::BasicBlock> {
        match self {
            SemeRegion::Block { .. } => {
                panic!("into_blocks on anonymous Block — should be Closed or Open")
            }
            SemeRegion::Open {
                mut blocks,
                entry: _,
                fallthrough,
                fallthrough_stmts,
            } => {
                blocks.insert(
                    fallthrough,
                    lang::BasicBlock {
                        statements: fallthrough_stmts.into_iter().collect(),
                        terminator: lang::Terminator::Unreachable,
                        kind: lang::BbKind::Regular,
                    },
                );
                blocks
            }
            SemeRegion::Closed { blocks, entry: _ } => blocks,
        }
    }

    // -----------------------------------------------------------------------
    // Assign (pure — no naming needed)
    // -----------------------------------------------------------------------

    pub fn assign(
        &self,
        dest: impl Upcast<MiniRustPlace>,
        value: impl Upcast<MiniRustValue>,
    ) -> Self {
        let dest: MiniRustPlace = dest.upcast();
        let value: MiniRustValue = value.upcast();
        self.clone().with_stmt(lang::Statement::Assign {
            destination: dest.into(),
            source: value.into(),
        })
    }
}

// ===========================================================================
// CodegenFn methods that operate on SemeRegion (these thread state properly)
// ===========================================================================

impl CodegenFn {
    /// Terminate a region's current fallthrough block. Allocates a bb name if
    /// the region is an anonymous Block. If `next_bb` is `Some`, transitions to
    /// Open with that as the new fallthrough; otherwise stays Closed.
    pub(super) fn terminate(
        &self,
        region: impl Upcast<SemeRegion>,
        terminator: impl Upcast<MiniRustTerminator>,
        next_bb: impl Upcast<Option<MiniRustBb>>,
    ) -> (SemeRegion, Self) {
        let region: SemeRegion = region.upcast();
        let terminator: MiniRustTerminator = terminator.upcast();
        let next_bb: Option<MiniRustBb> = next_bb.upcast();

        match region {
            SemeRegion::Block { stmts } => {
                let (name, cfn) = self.fresh_bb();
                let bb = lang::BasicBlock {
                    statements: stmts.into_iter().collect(),
                    terminator: terminator.upcast(),
                    kind: lang::BbKind::Regular,
                };
                let mut blocks = Map::new();
                blocks.insert(name, bb);
                match next_bb {
                    Some(next) => (
                        SemeRegion::Open {
                            blocks,
                            entry: name,
                            fallthrough: next.upcast(),
                            fallthrough_stmts: Vec::new(),
                        },
                        cfn,
                    ),
                    None => (
                        SemeRegion::Closed {
                            blocks,
                            entry: name,
                        },
                        cfn,
                    ),
                }
            }
            SemeRegion::Open {
                mut blocks,
                entry,
                fallthrough,
                fallthrough_stmts,
            } => {
                let bb = lang::BasicBlock {
                    statements: fallthrough_stmts.into_iter().collect(),
                    terminator: terminator.upcast(),
                    kind: lang::BbKind::Regular,
                };
                blocks.insert(fallthrough, bb);
                match next_bb {
                    Some(next) => (
                        SemeRegion::Open {
                            blocks,
                            entry,
                            fallthrough: next.upcast(),
                            fallthrough_stmts: Vec::new(),
                        },
                        self.clone(),
                    ),
                    None => (SemeRegion::Closed { blocks, entry }, self.clone()),
                }
            }
            SemeRegion::Closed { blocks, entry } => match next_bb {
                Some(next) => (
                    SemeRegion::Open {
                        blocks,
                        entry,
                        fallthrough: next.upcast(),
                        fallthrough_stmts: Vec::new(),
                    },
                    self.clone(),
                ),
                None => (SemeRegion::Closed { blocks, entry }, self.clone()),
            },
        }
    }

    /// Append `other` onto `region`.
    ///
    /// - Block + Block: concatenate statements (both anonymous).
    /// - anything + Open/Closed: terminate self's fallthrough with Goto to
    ///   other's entry, then merge all of other's blocks intact.
    pub(super) fn append(&self, region: SemeRegion, other: SemeRegion) -> (SemeRegion, Self) {
        if !region.has_fallthrough() {
            return (region, self.clone());
        }

        match (region, other) {
            // Block + Block: just concatenate statements, stay anonymous.
            (SemeRegion::Block { mut stmts }, SemeRegion::Block { stmts: other_stmts }) => {
                stmts.extend(other_stmts);
                (SemeRegion::Block { stmts }, self.clone())
            }

            // Block + Open/Closed: name self first, then emit Goto.
            (SemeRegion::Block { stmts }, other) => {
                let (name, cfn) = self.fresh_bb();
                let open = SemeRegion::Open {
                    blocks: Map::new(),
                    entry: name,
                    fallthrough: name,
                    fallthrough_stmts: stmts,
                };
                cfn.append_named(open, other)
            }

            // Open + Block: extend fallthrough statements.
            (
                SemeRegion::Open {
                    blocks,
                    entry,
                    fallthrough,
                    mut fallthrough_stmts,
                },
                SemeRegion::Block { stmts: other_stmts },
            ) => {
                fallthrough_stmts.extend(other_stmts);
                (
                    SemeRegion::Open {
                        blocks,
                        entry,
                        fallthrough,
                        fallthrough_stmts,
                    },
                    self.clone(),
                )
            }

            // Open + Open/Closed: emit Goto to other's entry.
            (region @ SemeRegion::Open { .. }, other) => self.append_named(region, other),

            (SemeRegion::Closed { .. }, _) => unreachable!("append called on Closed"),
        }
    }

    /// Append a named region (`other` is Open or Closed) onto `region` (which is Open).
    /// Terminates self's fallthrough with a Goto to other's entry, preserving all
    /// of other's blocks intact.
    fn append_named(&self, region: SemeRegion, other: SemeRegion) -> (SemeRegion, Self) {
        let (mut blocks, entry, ft_name, ft_stmts) = match region {
            SemeRegion::Open {
                blocks,
                entry,
                fallthrough,
                fallthrough_stmts,
            } => (blocks, entry, fallthrough, fallthrough_stmts),
            _ => unreachable!(),
        };

        let other_entry = other.entry();

        // Finalize self's fallthrough with Goto to other's entry.
        blocks.insert(
            ft_name,
            lang::BasicBlock {
                statements: ft_stmts.into_iter().collect(),
                terminator: lang::Terminator::Goto(other_entry),
                kind: lang::BbKind::Regular,
            },
        );

        match other {
            SemeRegion::Open {
                blocks: other_blocks,
                entry: _,
                fallthrough: other_ft,
                fallthrough_stmts: other_ft_stmts,
            } => {
                merge_blocks(&mut blocks, other_blocks);
                (
                    SemeRegion::Open {
                        blocks,
                        entry,
                        fallthrough: other_ft,
                        fallthrough_stmts: other_ft_stmts,
                    },
                    self.clone(),
                )
            }

            SemeRegion::Closed {
                blocks: other_blocks,
                entry: _,
            } => {
                merge_blocks(&mut blocks, other_blocks);
                (SemeRegion::Closed { blocks, entry }, self.clone())
            }

            _ => unreachable!(),
        }
    }

    /// Branch on a bool condition: switch, absorb then/else regions, open a join block.
    pub(super) fn branch_on_bool(
        &self,
        region: SemeRegion,
        condition_local: lang::LocalName,
        then_region: SemeRegion,
        else_region: SemeRegion,
    ) -> (SemeRegion, Self) {
        if !region.has_fallthrough() {
            return (region, self.clone());
        }

        let (mut blocks, entry, ft_name, ft_stmts, mut cfn) = match region {
            SemeRegion::Block { stmts } => {
                let (name, cfn) = self.fresh_bb();
                (Map::new(), name, name, stmts, cfn)
            }
            SemeRegion::Open {
                blocks,
                entry,
                fallthrough,
                fallthrough_stmts,
            } => (blocks, entry, fallthrough, fallthrough_stmts, self.clone()),
            SemeRegion::Closed { .. } => unreachable!(),
        };

        let (then_entry, cfn2) = Self::region_entry_or_alloc(&cfn, &then_region);
        cfn = cfn2;
        let (else_entry, cfn2) = Self::region_entry_or_alloc(&cfn, &else_region);
        cfn = cfn2;
        let (join, cfn2) = cfn.fresh_bb();
        cfn = cfn2;

        let mut cases = Map::new();
        cases.insert(Int::from(1u8), then_entry);

        let bool_as_int_ty = lang::Type::Int(lang::IntType {
            signed: Signedness::Unsigned,
            size: Size::from_bytes_const(1),
        });
        let cast_expr = lang::ValueExpr::UnOp {
            operator: lang::UnOp::Cast(lang::CastOp::Transmute(bool_as_int_ty)),
            operand: GcCow::new(lang::ValueExpr::Load {
                source: GcCow::new(lang::PlaceExpr::Local(condition_local)),
            }),
        };

        blocks.insert(
            ft_name,
            lang::BasicBlock {
                statements: ft_stmts.into_iter().collect(),
                terminator: lang::Terminator::Switch {
                    value: cast_expr,
                    cases,
                    fallback: else_entry,
                },
                kind: lang::BbKind::Regular,
            },
        );

        let then_has_ft = then_region.has_fallthrough();
        let else_has_ft = else_region.has_fallthrough();

        Self::drain_region_into(&mut blocks, then_region, then_entry, join);
        Self::drain_region_into(&mut blocks, else_region, else_entry, join);

        if then_has_ft || else_has_ft {
            (
                SemeRegion::Open {
                    blocks,
                    entry,
                    fallthrough: join,
                    fallthrough_stmts: Vec::new(),
                },
                cfn,
            )
        } else {
            (SemeRegion::Closed { blocks, entry }, cfn)
        }
    }

    fn region_entry_or_alloc(cfn: &CodegenFn, region: &SemeRegion) -> (lang::BbName, CodegenFn) {
        match region {
            SemeRegion::Block { .. } => cfn.fresh_bb(),
            SemeRegion::Open { entry, .. } => (*entry, cfn.clone()),
            SemeRegion::Closed { entry, .. } => (*entry, cfn.clone()),
        }
    }

    fn drain_region_into(
        blocks: &mut Map<lang::BbName, lang::BasicBlock>,
        region: SemeRegion,
        name: lang::BbName,
        goto: lang::BbName,
    ) {
        match region {
            SemeRegion::Block { stmts } => {
                blocks.insert(
                    name,
                    lang::BasicBlock {
                        statements: stmts.into_iter().collect(),
                        terminator: lang::Terminator::Goto(goto),
                        kind: lang::BbKind::Regular,
                    },
                );
            }
            SemeRegion::Open {
                blocks: region_blocks,
                entry: _,
                fallthrough,
                fallthrough_stmts,
            } => {
                merge_blocks(blocks, region_blocks);
                blocks.insert(
                    fallthrough,
                    lang::BasicBlock {
                        statements: fallthrough_stmts.into_iter().collect(),
                        terminator: lang::Terminator::Goto(goto),
                        kind: lang::BbKind::Regular,
                    },
                );
            }
            SemeRegion::Closed {
                blocks: region_blocks,
                entry: _,
            } => {
                merge_blocks(blocks, region_blocks);
            }
        }
    }

    // -----------------------------------------------------------------------
    // Compound builders (codegen-specific)
    // -----------------------------------------------------------------------

    /// Append `other` onto `region`, threading state.
    pub(super) fn append_from(
        &self,
        region: &SemeRegion,
        other: impl Upcast<SemeRegion>,
    ) -> (SemeRegion, Self) {
        self.append(region.clone(), other.upcast())
    }

    /// Terminate and return the result (closed — no next block).
    pub(super) fn terminated(
        &self,
        region: &SemeRegion,
        terminator: impl Upcast<MiniRustTerminator>,
    ) -> (SemeRegion, Self) {
        self.terminate(region.clone(), terminator, ())
    }

    /// Build a Call terminator, allocate the next block.
    pub(super) fn call(
        &self,
        region: &SemeRegion,
        fn_name: impl Upcast<MiniRustFn>,
        args: &[MiniRustLocal],
        ret: impl Upcast<MiniRustLocal>,
    ) -> Fallible<(SemeRegion, Self)> {
        let fn_name: MiniRustFn = fn_name.upcast();
        let ret: MiniRustLocal = ret.upcast();
        let arg_exprs: List<lang::ArgumentExpr> = args
            .iter()
            .map(|t| {
                lang::ArgumentExpr::ByValue(lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(t.0)),
                })
            })
            .collect();
        let (next_bb, cfn) = self.fresh_bb();
        let (region, cfn) = cfn.terminate(
            region.clone(),
            lang::Terminator::Call {
                callee: lang::ValueExpr::Constant(
                    lang::Constant::FnPointer(fn_name.into()),
                    lang::Type::Ptr(PtrType::FnPtr),
                ),
                calling_convention: lang::CallingConvention::Rust,
                arguments: arg_exprs,
                ret: lang::PlaceExpr::Local(ret.into()),
                next_block: Some(next_bb),
                unwind_block: None,
            },
            Some(next_bb),
        );
        Ok((region, cfn))
    }

    /// Build a branch-on-bool from a region.
    pub(super) fn branch_on_bool_from(
        &self,
        region: &SemeRegion,
        cond: impl Upcast<MiniRustLocal>,
        then_r: impl Upcast<SemeRegion>,
        else_r: impl Upcast<SemeRegion>,
    ) -> (SemeRegion, Self) {
        let cond: MiniRustLocal = cond.upcast();
        self.branch_on_bool(
            region.clone(),
            cond.into(),
            then_r.upcast(),
            else_r.upcast(),
        )
    }

    /// Build a print intrinsic call.
    pub(super) fn print_intrinsic(
        &self,
        region: &SemeRegion,
        value: impl Upcast<MiniRustLocal>,
    ) -> Fallible<(SemeRegion, Self)> {
        let value: MiniRustLocal = value.upcast();
        let (next_bb, cfn) = self.fresh_bb();
        let (print_ret, cfn) = cfn.alloc_local(unit_ty());
        let (region, cfn) = cfn.terminate(
            region.clone(),
            lang::Terminator::Intrinsic {
                intrinsic: lang::IntrinsicOp::PrintStdout,
                arguments: list![lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(value.into()))
                }],
                ret: lang::PlaceExpr::Local(print_ret),
                next_block: Some(next_bb),
            },
            Some(next_bb),
        );
        Ok((region, cfn))
    }
}
