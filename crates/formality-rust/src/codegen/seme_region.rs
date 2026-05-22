use libspecr::hidden::GcCow;
use libspecr::prelude::*;
use minirust_rs::lang;

/// A single-entry, multi-exit control flow region.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SemeRegion {
    blocks: Vec<(lang::BbName, lang::BasicBlock)>,
    entry: lang::BbName,
    fallthrough_stmts: Vec<lang::Statement>,
    fallthrough: Option<lang::BbName>,
}

impl SemeRegion {
    pub fn empty(entry: &lang::BbName) -> Self {
        SemeRegion {
            blocks: Vec::new(),
            entry: *entry,
            fallthrough_stmts: Vec::new(),
            fallthrough: Some(*entry),
        }
    }

    pub fn push_stmt(mut self, stmt: lang::Statement) -> Self {
        if self.fallthrough.is_some() {
            self.fallthrough_stmts.push(stmt);
        }
        self
    }

    pub fn has_fallthrough(&self) -> bool {
        self.fallthrough.is_some()
    }

    pub fn entry(&self) -> lang::BbName {
        self.entry
    }

    pub fn terminate(mut self, terminator: lang::Terminator) -> Self {
        if let Some(bb_name) = self.fallthrough.take() {
            let stmts: List<lang::Statement> =
                self.fallthrough_stmts.drain(..).collect();
            self.blocks.push((
                bb_name,
                lang::BasicBlock {
                    statements: stmts,
                    terminator,
                    kind: lang::BbKind::Regular,
                },
            ));
        }
        self
    }

    pub fn add_empty_block(mut self, new_bb: lang::BbName) -> Self {
        self.fallthrough = Some(new_bb);
        self.fallthrough_stmts.clear();
        self
    }

    pub fn append(
        mut self,
        other: SemeRegion,
        fresh_bb: impl FnOnce() -> lang::BbName,
    ) -> SemeRegion {
        match self.fallthrough {
            Some(_) => {
                let entry_name = other.entry;
                let mut other_blocks = other.blocks;
                let mut entry_block = None;

                other_blocks.retain(|(name, blk)| {
                    if *name == entry_name {
                        entry_block = Some(blk.clone());
                        false
                    } else {
                        true
                    }
                });

                if let Some(blk) = entry_block {
                    for s in blk.statements {
                        self.fallthrough_stmts.push(s);
                    }
                    let ft = self.fallthrough.take().unwrap();
                    let all_stmts: List<lang::Statement> =
                        self.fallthrough_stmts.drain(..).collect();
                    self.blocks.push((
                        ft,
                        lang::BasicBlock {
                            statements: all_stmts,
                            terminator: blk.terminator,
                            kind: lang::BbKind::Regular,
                        },
                    ));
                    self.blocks.extend(other_blocks);
                    self.fallthrough = other.fallthrough;
                    if let Some(ref ft) = self.fallthrough {
                        if *ft == entry_name {
                            self.fallthrough = Some(fresh_bb());
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
        condition_local: lang::LocalName,
        then_region: SemeRegion,
        else_region: SemeRegion,
        join_bb: lang::BbName,
    ) -> SemeRegion {
        if self.fallthrough.is_none() {
            return self;
        }

        let then_entry = then_region.entry;
        let else_entry = else_region.entry;

        let ft = self.fallthrough.take().unwrap();
        let ft_stmts: List<lang::Statement> =
            self.fallthrough_stmts.drain(..).collect();

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

        // Add then blocks
        self.blocks.extend(then_region.blocks);
        if let Some(ft) = then_region.fallthrough {
            let stmts: List<lang::Statement> = then_region
                .fallthrough_stmts
                .into_iter()
                .collect();
            self.blocks.push((
                ft,
                lang::BasicBlock {
                    statements: stmts,
                    terminator: lang::Terminator::Goto(join_bb),
                    kind: lang::BbKind::Regular,
                },
            ));
        }

        // Add else blocks
        self.blocks.extend(else_region.blocks);
        if let Some(ft) = else_region.fallthrough {
            let stmts: List<lang::Statement> = else_region
                .fallthrough_stmts
                .into_iter()
                .collect();
            self.blocks.push((
                ft,
                lang::BasicBlock {
                    statements: stmts,
                    terminator: lang::Terminator::Goto(join_bb),
                    kind: lang::BbKind::Regular,
                },
            ));
        }

        let has_join = then_region.fallthrough.is_some() || else_region.fallthrough.is_some();
        self.fallthrough = if has_join { Some(join_bb) } else { None };
        self.fallthrough_stmts = Vec::new();
        self
    }

    pub fn into_blocks(mut self) -> Vec<(lang::BbName, lang::BasicBlock)> {
        if let Some(ft) = self.fallthrough.take() {
            let stmts: List<lang::Statement> =
                self.fallthrough_stmts.drain(..).collect();
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
