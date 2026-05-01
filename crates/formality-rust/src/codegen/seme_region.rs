use libspecr::hidden::GcCow;
use libspecr::prelude::*;
use minirust_rs::lang;

use super::CodegenState;

/// A single-entry, multi-exit control flow region.
pub struct SemeRegion {
    /// Completed basic blocks (all have terminators).
    blocks: Vec<(lang::BbName, lang::BasicBlock)>,
    /// The entry block name.
    entry: lang::BbName,
    /// Statements accumulated in the current fallthrough block.
    fallthrough_stmts: Vec<lang::Statement>,
    /// The fallthrough block name, or None if all paths diverged.
    fallthrough: Option<lang::BbName>,
}

impl SemeRegion {
    /// Create an empty region with a fresh entry block.
    pub fn empty(state: &mut CodegenState) -> Self {
        let entry = state.fresh_bb();
        SemeRegion {
            blocks: Vec::new(),
            entry,
            fallthrough_stmts: Vec::new(),
            fallthrough: Some(entry),
        }
    }

    /// Push a statement into the current fallthrough block.
    /// No-op if there is no fallthrough (dead code).
    pub fn push_stmt(&mut self, stmt: lang::Statement) {
        if self.fallthrough.is_some() {
            self.fallthrough_stmts.push(stmt);
        }
    }

    /// Whether this region has a fallthrough (i.e., not all paths diverged).
    pub fn has_fallthrough(&self) -> bool {
        self.fallthrough.is_some()
    }

    /// The entry block name.
    pub fn entry(&self) -> lang::BbName {
        self.entry
    }

    /// Terminate the current fallthrough block.
    /// After this, fallthrough becomes None (diverged).
    pub fn terminate(&mut self, terminator: lang::Terminator) {
        if let Some(bb_name) = self.fallthrough.take() {
            let stmts: List<lang::Statement> = self.fallthrough_stmts.drain(..).collect();
            self.blocks.push((
                bb_name,
                lang::BasicBlock {
                    statements: stmts,
                    terminator,
                    kind: lang::BbKind::Regular,
                },
            ));
        }
    }

    /// Add an empty block as the new fallthrough.
    /// Used after terminators that branch to a continuation.
    pub fn add_empty_block(&mut self, bb: lang::BbName) {
        self.fallthrough = Some(bb);
        self.fallthrough_stmts.clear();
    }

    /// Sequence two regions: self followed by other.
    pub fn append(mut self, state: &mut CodegenState, other: SemeRegion) -> SemeRegion {
        match self.fallthrough {
            Some(_ft) => {
                // Inline other's entry statements into our fallthrough.
                // The entry block of `other` has no incoming edges from outside,
                // so we can safely inline its statements.
                //
                // other.entry's statements are in other.fallthrough_stmts if
                // other.entry == other.fallthrough, or in other.blocks otherwise.

                // Find and remove the entry block from other's completed blocks
                let entry_name = other.entry;
                let mut other_blocks = other.blocks;
                let mut entry_stmts_from_completed = None;
                let mut entry_terminator = None;

                other_blocks.retain(|(name, bb)| {
                    if *name == entry_name {
                        entry_stmts_from_completed = Some(bb.statements.clone());
                        entry_terminator = Some(bb.terminator);
                        false
                    } else {
                        true
                    }
                });

                if let Some(stmts) = entry_stmts_from_completed {
                    // Entry was a completed block — inline its stmts and terminator
                    for s in stmts {
                        self.fallthrough_stmts.push(s);
                    }
                    // Terminate our fallthrough with the entry's terminator
                    let ft = self.fallthrough.take().unwrap();
                    let all_stmts: List<lang::Statement> =
                        self.fallthrough_stmts.drain(..).collect();
                    self.blocks.push((
                        ft,
                        lang::BasicBlock {
                            statements: all_stmts,
                            terminator: entry_terminator.unwrap(),
                            kind: lang::BbKind::Regular,
                        },
                    ));
                    self.blocks.extend(other_blocks);
                    self.fallthrough = other.fallthrough;
                    if let Some(ft) = other.fallthrough {
                        if ft == entry_name {
                            // other's fallthrough was its entry, which we inlined.
                            // We need a new fallthrough.
                            let new_ft = state.fresh_bb();
                            self.fallthrough = Some(new_ft);
                        }
                    }
                    self.fallthrough_stmts = other.fallthrough_stmts;
                } else {
                    // Entry was the fallthrough of other (not yet completed).
                    // Inline other's pending stmts into our fallthrough.
                    // self.fallthrough stays the same (we inlined into it).
                    self.fallthrough_stmts.extend(other.fallthrough_stmts);
                    self.blocks.extend(other_blocks);
                    // fallthrough stays as self.fallthrough
                }

                self
            }
            None => {
                // Dead code — discard other entirely.
                self
            }
        }
    }

    /// Branch with a boolean condition value.
    pub fn branch_on_bool(
        mut self,
        state: &mut CodegenState,
        condition_local: lang::LocalName,
        then_region: SemeRegion,
        else_region: SemeRegion,
    ) -> SemeRegion {
        if self.fallthrough.is_none() {
            return self;
        }

        let then_entry = then_region.entry;
        let else_entry = else_region.entry;
        let join = state.fresh_bb();

        // Terminate condition's fallthrough with Switch
        let ft = self.fallthrough.take().unwrap();
        let ft_stmts: List<lang::Statement> = self.fallthrough_stmts.drain(..).collect();

        let mut cases = Map::new();
        cases.insert(Int::from(1u8), then_entry); // true → then

        // MiniRust Switch requires Int, so cast bool to u8
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

        self.blocks.push((
            ft,
            lang::BasicBlock {
                statements: ft_stmts,
                terminator: lang::Terminator::Switch {
                    value: cast_expr,
                    cases,
                    fallback: else_entry, // false → else
                },
                kind: lang::BbKind::Regular,
            },
        ));

        // Add then blocks
        self.blocks.extend(then_region.blocks);
        // Terminate then's fallthrough with Goto(join)
        if let Some(ft) = then_region.fallthrough {
            let stmts: List<lang::Statement> = then_region.fallthrough_stmts.into_iter().collect();
            self.blocks.push((
                ft,
                lang::BasicBlock {
                    statements: stmts,
                    terminator: lang::Terminator::Goto(join),
                    kind: lang::BbKind::Regular,
                },
            ));
        }

        // Add else blocks
        self.blocks.extend(else_region.blocks);
        // Terminate else's fallthrough with Goto(join)
        if let Some(ft) = else_region.fallthrough {
            let stmts: List<lang::Statement> = else_region.fallthrough_stmts.into_iter().collect();
            self.blocks.push((
                ft,
                lang::BasicBlock {
                    statements: stmts,
                    terminator: lang::Terminator::Goto(join),
                    kind: lang::BbKind::Regular,
                },
            ));
        }

        let has_join = then_region.fallthrough.is_some() || else_region.fallthrough.is_some();

        self.fallthrough = if has_join { Some(join) } else { None };
        self.fallthrough_stmts = Vec::new();
        self
    }

    /// Convert to a list of completed basic blocks.
    /// If there's still a fallthrough, terminate it with Unreachable.
    pub fn into_blocks(mut self) -> Vec<(lang::BbName, lang::BasicBlock)> {
        if let Some(ft) = self.fallthrough.take() {
            let stmts: List<lang::Statement> = self.fallthrough_stmts.drain(..).collect();
            self.blocks.push((
                ft,
                lang::BasicBlock {
                    statements: stmts,
                    terminator: lang::Terminator::Unreachable,
                    kind: lang::BbKind::Regular,
                },
            ));
        }
        self.blocks
    }
}
