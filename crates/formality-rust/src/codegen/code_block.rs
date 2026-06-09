use crate::grammar::Fallible;
use formality_core::Upcast;
use libspecr::hidden::GcCow;
use libspecr::list;
use libspecr::prelude::*;
use minirust_rs::lang;
use minirust_rs::mem::PtrType;

use super::minirust::*;
use super::scope::CodegenFn;

// ANCHOR: CodeBlock
/// A single-entry, multi-exit control flow code.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum CodeBlock {
    /// Anonymous open block being constructed. Has no name yet — a name is
    /// allocated lazily when the block is terminated or needs to be referenced.
    ///
    /// Suppose you have `let x = 22;` this would create a set of statements like
    ///
    /// ```
    /// stmts: [
    ///   let tmp1;
    ///   tmp1 = 22;
    ///   let x;
    ///   x = tmp;
    /// ]
    /// ```
    ///
    /// and then you have `let y = 33;`
    ///
    /// ```
    /// stmts: [
    ///   let tmp2;
    ///   tmp2 = 33;
    ///   let y;
    ///   y = tmp2;
    /// ]
    /// ```
    ///
    /// and then you append those two so you get
    ///
    /// ```
    /// stmts = [... /* as above, concatenated */]
    /// ```
    Block { stmts: Vec<lang::Statement> },

    /// Finalized blocks plus one open fallthrough block.
    ///
    /// For something like this `if foo { 22 } else { 33 }; ...``
    ///
    /// ```text
    /// blocks:
    ///   a {
    ///     if foo goto b else c
    ///   }
    ///   b {
    ///      tmp = 22
    ///      goto d
    ///   }
    ///   c {
    ///     tmp = 33
    ///     goto d
    ///   }
    /// entry: a
    /// fallthrough: d
    /// fallthrough_stmts: [...] // from the `...` above
    /// }}
    /// ```
    Open {
        /// Blocks that have a terminator and thus are "done".
        /// They may branch to each other or to the (incomplete) fallthrough block, as shown above.
        blocks: Map<lang::BbName, lang::BasicBlock>,

        /// The entry block in the list above or the fallthrough.
        entry: lang::BbName,

        /// The name of the not-yet-compelted fallthrough block, which may be referenced above.
        fallthrough: lang::BbName,

        /// The statements in the fallthrough block so far.
        fallthrough_stmts: Vec<lang::Statement>,
    },

    /// Finalized blocks (i.e., blocks with terminators) only. No successors.
    Closed {
        /// Blocks that have a terminator and thus are "done".
        blocks: Map<lang::BbName, lang::BasicBlock>,

        /// The entry block in the list above or the fallthrough.
        entry: lang::BbName,
    },
}
// ANCHOR_END: CodeBlock

fn merge_blocks(
    target: &mut Map<lang::BbName, lang::BasicBlock>,
    source: Map<lang::BbName, lang::BasicBlock>,
) {
    for (k, v) in source {
        target.insert(k, v);
    }
}

impl CodeBlock {
    pub fn new() -> Self {
        CodeBlock::Block { stmts: Vec::new() }
    }

    /// Create a named code — already in Open state with the given bb as
    /// both entry and fallthrough.
    pub fn named(bb: lang::BbName) -> Self {
        CodeBlock::Open {
            blocks: Map::new(),
            entry: bb,
            fallthrough: bb,
            fallthrough_stmts: Vec::new(),
        }
    }

    pub fn with_stmt(self, stmt: lang::Statement) -> Self {
        match self {
            CodeBlock::Block { mut stmts } => {
                stmts.push(stmt);
                CodeBlock::Block { stmts }
            }
            CodeBlock::Open {
                blocks,
                entry,
                fallthrough,
                mut fallthrough_stmts,
            } => {
                fallthrough_stmts.push(stmt);
                CodeBlock::Open {
                    blocks,
                    entry,
                    fallthrough,
                    fallthrough_stmts,
                }
            }
            closed @ CodeBlock::Closed { .. } => closed,
        }
    }

    pub fn has_fallthrough(&self) -> bool {
        matches!(self, CodeBlock::Block { .. } | CodeBlock::Open { .. })
    }

    pub fn entry(&self) -> lang::BbName {
        match self {
            CodeBlock::Block { .. } => panic!("entry() called on anonymous Block"),
            CodeBlock::Open { entry, .. } => *entry,
            CodeBlock::Closed { entry, .. } => *entry,
        }
    }

    /// Finalize into basic blocks. Panics if called on an anonymous Block.
    pub fn into_blocks(self) -> Map<lang::BbName, lang::BasicBlock> {
        match self {
            CodeBlock::Block { .. } => {
                panic!("into_blocks on anonymous Block — should be Closed or Open")
            }
            CodeBlock::Open {
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
            CodeBlock::Closed { blocks, entry: _ } => blocks,
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
// CodegenFn methods that operate on CodeBlock (these thread state properly)
// ===========================================================================

impl CodegenFn {
    /// Terminate a code's current fallthrough block. Allocates a bb name if
    /// the code is an anonymous Block. If `next_bb` is `Some`, transitions to
    /// Open with that as the new fallthrough; otherwise stays Closed.
    pub(super) fn terminate(
        &self,
        code: impl Upcast<CodeBlock>,
        terminator: impl Upcast<MiniRustTerminator>,
        next_bb: impl Upcast<Option<MiniRustBb>>,
    ) -> (CodeBlock, Self) {
        let code: CodeBlock = code.upcast();
        let terminator: MiniRustTerminator = terminator.upcast();
        let next_bb: Option<MiniRustBb> = next_bb.upcast();

        match code {
            CodeBlock::Block { stmts } => {
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
                        CodeBlock::Open {
                            blocks,
                            entry: name,
                            fallthrough: next.upcast(),
                            fallthrough_stmts: Vec::new(),
                        },
                        cfn,
                    ),
                    None => (
                        CodeBlock::Closed {
                            blocks,
                            entry: name,
                        },
                        cfn,
                    ),
                }
            }
            CodeBlock::Open {
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
                        CodeBlock::Open {
                            blocks,
                            entry,
                            fallthrough: next.upcast(),
                            fallthrough_stmts: Vec::new(),
                        },
                        self.clone(),
                    ),
                    None => (CodeBlock::Closed { blocks, entry }, self.clone()),
                }
            }
            CodeBlock::Closed { blocks, entry } => match next_bb {
                Some(next) => (
                    CodeBlock::Open {
                        blocks,
                        entry,
                        fallthrough: next.upcast(),
                        fallthrough_stmts: Vec::new(),
                    },
                    self.clone(),
                ),
                None => (CodeBlock::Closed { blocks, entry }, self.clone()),
            },
        }
    }

    /// Append `other` onto `code`.
    ///
    /// - Block + Block: concatenate statements (both anonymous).
    /// - anything + Open/Closed: terminate self's fallthrough with Goto to
    ///   other's entry, then merge all of other's blocks intact.
    pub(super) fn append(&self, code: CodeBlock, other: CodeBlock) -> (CodeBlock, Self) {
        if !code.has_fallthrough() {
            return (code, self.clone());
        }

        match (code, other) {
            // Block + Block: just concatenate statements, stay anonymous.
            (CodeBlock::Block { mut stmts }, CodeBlock::Block { stmts: other_stmts }) => {
                stmts.extend(other_stmts);
                (CodeBlock::Block { stmts }, self.clone())
            }

            // Block + Open/Closed: name self first, then emit Goto.
            (CodeBlock::Block { stmts }, other) => {
                let (name, cfn) = self.fresh_bb();
                let open = CodeBlock::Open {
                    blocks: Map::new(),
                    entry: name,
                    fallthrough: name,
                    fallthrough_stmts: stmts,
                };
                cfn.append_named(open, other)
            }

            // Open + Block: extend fallthrough statements.
            (
                CodeBlock::Open {
                    blocks,
                    entry,
                    fallthrough,
                    mut fallthrough_stmts,
                },
                CodeBlock::Block { stmts: other_stmts },
            ) => {
                fallthrough_stmts.extend(other_stmts);
                (
                    CodeBlock::Open {
                        blocks,
                        entry,
                        fallthrough,
                        fallthrough_stmts,
                    },
                    self.clone(),
                )
            }

            // Open + Open/Closed: emit Goto to other's entry.
            (code @ CodeBlock::Open { .. }, other) => self.append_named(code, other),

            (CodeBlock::Closed { .. }, _) => unreachable!("append called on Closed"),
        }
    }

    /// Append a named code (`other` is Open or Closed) onto `code` (which is Open).
    /// Terminates self's fallthrough with a Goto to other's entry, preserving all
    /// of other's blocks intact.
    fn append_named(&self, code: CodeBlock, other: CodeBlock) -> (CodeBlock, Self) {
        let (mut blocks, entry, ft_name, ft_stmts) = match code {
            CodeBlock::Open {
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
            CodeBlock::Open {
                blocks: other_blocks,
                entry: _,
                fallthrough: other_ft,
                fallthrough_stmts: other_ft_stmts,
            } => {
                merge_blocks(&mut blocks, other_blocks);
                (
                    CodeBlock::Open {
                        blocks,
                        entry,
                        fallthrough: other_ft,
                        fallthrough_stmts: other_ft_stmts,
                    },
                    self.clone(),
                )
            }

            CodeBlock::Closed {
                blocks: other_blocks,
                entry: _,
            } => {
                merge_blocks(&mut blocks, other_blocks);
                (CodeBlock::Closed { blocks, entry }, self.clone())
            }

            _ => unreachable!(),
        }
    }

    /// Branch on a bool condition: switch, absorb then/else regions, open a join block.
    pub(super) fn branch_on_bool(
        &self,
        code: CodeBlock,
        condition_local: lang::LocalName,
        then_region: CodeBlock,
        else_region: CodeBlock,
    ) -> (CodeBlock, Self) {
        if !code.has_fallthrough() {
            return (code, self.clone());
        }

        let (mut blocks, entry, ft_name, ft_stmts, mut cfn) = match code {
            CodeBlock::Block { stmts } => {
                let (name, cfn) = self.fresh_bb();
                (Map::new(), name, name, stmts, cfn)
            }
            CodeBlock::Open {
                blocks,
                entry,
                fallthrough,
                fallthrough_stmts,
            } => (blocks, entry, fallthrough, fallthrough_stmts, self.clone()),
            CodeBlock::Closed { .. } => unreachable!(),
        };

        let (then_entry, cfn2) = Self::block_entry_or_alloc(&cfn, &then_region);
        cfn = cfn2;
        let (else_entry, cfn2) = Self::block_entry_or_alloc(&cfn, &else_region);
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

        Self::drain_block_into(&mut blocks, then_region, then_entry, join);
        Self::drain_block_into(&mut blocks, else_region, else_entry, join);

        if then_has_ft || else_has_ft {
            (
                CodeBlock::Open {
                    blocks,
                    entry,
                    fallthrough: join,
                    fallthrough_stmts: Vec::new(),
                },
                cfn,
            )
        } else {
            (CodeBlock::Closed { blocks, entry }, cfn)
        }
    }

    fn block_entry_or_alloc(cfn: &CodegenFn, code: &CodeBlock) -> (lang::BbName, CodegenFn) {
        match code {
            CodeBlock::Block { .. } => cfn.fresh_bb(),
            CodeBlock::Open { entry, .. } => (*entry, cfn.clone()),
            CodeBlock::Closed { entry, .. } => (*entry, cfn.clone()),
        }
    }

    fn drain_block_into(
        blocks: &mut Map<lang::BbName, lang::BasicBlock>,
        code: CodeBlock,
        name: lang::BbName,
        goto: lang::BbName,
    ) {
        match code {
            CodeBlock::Block { stmts } => {
                blocks.insert(
                    name,
                    lang::BasicBlock {
                        statements: stmts.into_iter().collect(),
                        terminator: lang::Terminator::Goto(goto),
                        kind: lang::BbKind::Regular,
                    },
                );
            }
            CodeBlock::Open {
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
            CodeBlock::Closed {
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

    /// Append `other` onto `code`, threading state.
    pub(super) fn append_from(
        &self,
        code: &CodeBlock,
        other: impl Upcast<CodeBlock>,
    ) -> (CodeBlock, Self) {
        self.append(code.clone(), other.upcast())
    }

    /// Terminate and return the result (closed — no next block).
    pub(super) fn terminated(
        &self,
        code: &CodeBlock,
        terminator: impl Upcast<MiniRustTerminator>,
    ) -> (CodeBlock, Self) {
        self.terminate(code.clone(), terminator, ())
    }

    /// Build a Call terminator, allocate the next block.
    pub(super) fn call(
        &self,
        code: &CodeBlock,
        fn_name: impl Upcast<MiniRustFn>,
        args: &[MiniRustLocal],
        ret: impl Upcast<MiniRustLocal>,
    ) -> Fallible<(CodeBlock, Self)> {
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
        let (code, cfn) = cfn.terminate(
            code.clone(),
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
        Ok((code, cfn))
    }

    /// Build a branch-on-bool from a code.
    pub(super) fn branch_on_bool_from(
        &self,
        code: &CodeBlock,
        cond: impl Upcast<MiniRustLocal>,
        then_r: impl Upcast<CodeBlock>,
        else_r: impl Upcast<CodeBlock>,
    ) -> (CodeBlock, Self) {
        let cond: MiniRustLocal = cond.upcast();
        self.branch_on_bool(code.clone(), cond.into(), then_r.upcast(), else_r.upcast())
    }

    /// Build a print intrinsic call.
    pub(super) fn print_intrinsic(
        &self,
        code: &CodeBlock,
        value: impl Upcast<MiniRustLocal>,
    ) -> Fallible<(CodeBlock, Self)> {
        let value: MiniRustLocal = value.upcast();
        let (next_bb, cfn) = self.fresh_bb();
        let (print_ret, cfn) = cfn.alloc_local(unit_ty());
        let (code, cfn) = cfn.terminate(
            code.clone(),
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
        Ok((code, cfn))
    }
}
