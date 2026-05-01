use libspecr::hidden::GcCow;
use libspecr::prelude::*;
use minirust_rs::lang;

use super::CodegenGlobal;

/// A single-entry, multi-exit control flow region.
pub struct SemeRegion {
    blocks: Vec<(lang::BbName, lang::BasicBlock)>,
    entry: lang::BbName,
    fallthrough_stmts: Vec<lang::Statement>,
    fallthrough: Option<lang::BbName>,
}

impl SemeRegion {
    pub fn empty(global: &mut CodegenGlobal) -> Self {
        let entry = global.fresh_bb();
        SemeRegion {
            blocks: Vec::new(),
            entry,
            fallthrough_stmts: Vec::new(),
            fallthrough: Some(entry),
        }
    }

    pub fn push_stmt(&mut self, stmt: lang::Statement) {
        if self.fallthrough.is_some() {
            self.fallthrough_stmts.push(stmt);
        }
    }

    pub fn has_fallthrough(&self) -> bool {
        self.fallthrough.is_some()
    }

    pub fn entry(&self) -> lang::BbName {
        self.entry
    }

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

    pub fn add_empty_block(&mut self, bb: lang::BbName) {
        self.fallthrough = Some(bb);
        self.fallthrough_stmts.clear();
    }

    pub fn append(mut self, global: &mut CodegenGlobal, other: SemeRegion) -> SemeRegion {
        match self.fallthrough {
            Some(_) => {
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
                    for s in stmts {
                        self.fallthrough_stmts.push(s);
                    }
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
                            let new_ft = global.fresh_bb();
                            self.fallthrough = Some(new_ft);
                        }
                    }
                    self.fallthrough_stmts = other.fallthrough_stmts;
                } else {
                    self.fallthrough_stmts.extend(other.fallthrough_stmts);
                    self.blocks.extend(other_blocks);
                }

                self
            }
            None => self,
        }
    }

    pub fn branch_on_bool(
        mut self,
        global: &mut CodegenGlobal,
        condition_local: lang::LocalName,
        then_region: SemeRegion,
        else_region: SemeRegion,
    ) -> SemeRegion {
        if self.fallthrough.is_none() {
            return self;
        }

        let then_entry = then_region.entry;
        let else_entry = else_region.entry;
        let join = global.fresh_bb();

        let ft = self.fallthrough.take().unwrap();
        let ft_stmts: List<lang::Statement> = self.fallthrough_stmts.drain(..).collect();

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

        self.blocks.push((
            ft,
            lang::BasicBlock {
                statements: ft_stmts,
                terminator: lang::Terminator::Switch {
                    value: cast_expr,
                    cases,
                    fallback: else_entry,
                },
                kind: lang::BbKind::Regular,
            },
        ));

        self.blocks.extend(then_region.blocks);
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

        self.blocks.extend(else_region.blocks);
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
