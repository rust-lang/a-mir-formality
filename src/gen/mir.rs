use rustc_middle::mir;

use crate::gen::FormalityGen;
use crate::renumber_mir;

impl<'tcx> FormalityGen<'tcx> {
    fn emit_place(&self, place: &mir::Place) -> String {
        let local = format!("_{}", place.local.index());
        place
            .iter_projections()
            .fold(local, |place, (_, proj)| match proj {
                mir::ProjectionElem::Deref => format!("(* {place})"),
                mir::ProjectionElem::Field(field, _) => {
                    format!("(field {place} {})", field.index())
                }
                mir::ProjectionElem::Index(l) => format!("(index {place} {})", l.index()),
                mir::ProjectionElem::Downcast(_, variant) => {
                    format!("(downcast {place} {})", variant.index())
                }
                _ => unimplemented!(),
            })
    }

    fn emit_operand(&self, operand: &mir::Operand) -> String {
        match operand {
            mir::Operand::Copy(place) => format!("(copy {})", self.emit_place(place)),
            mir::Operand::Move(place) => format!("(move {})", self.emit_place(place)),
            mir::Operand::Constant(ct) => {
                if let Some(int) = ct.literal.try_to_scalar().and_then(|s| s.try_to_int().ok()) {
                    format!("(const {})", int)
                } else {
                    eprintln!("unknown const: {ct}");
                    "(const 0)".to_string()
                }
            }
        }
    }

    fn emit_rvalue(&self, rvalue: &mir::Rvalue<'tcx>) -> String {
        match rvalue {
            mir::Rvalue::Use(operand) => format!("(use {})", self.emit_operand(operand)),
            mir::Rvalue::Repeat(operand, ct) => {
                format!(
                    "(repeat {} {})",
                    self.emit_operand(operand),
                    ct.kind()
                        .try_to_scalar_int()
                        .unwrap_or_else(|| unimplemented!())
                )
            }
            mir::Rvalue::Ref(re, bk, place) => {
                let mutability = match bk {
                    mir::BorrowKind::Shared => "()",
                    mir::BorrowKind::Mut { .. } => "mut",
                    _ => unimplemented!(),
                };
                format!(
                    "(ref {} {mutability} {})",
                    self.emit_lifetime(*re),
                    self.emit_place(place)
                )
            }
            mir::Rvalue::AddressOf(mu, place) => {
                let mutability = match mu {
                    mir::Mutability::Not => "()",
                    mir::Mutability::Mut => "mut",
                };
                format!("(addr-of {mutability} {})", self.emit_place(place))
            }
            mir::Rvalue::Len(place) => format!("(len {})", self.emit_place(place)),
            mir::Rvalue::BinaryOp(bin_op, operands) => {
                let op = match bin_op {
                    mir::BinOp::Add => "+",
                    mir::BinOp::Sub => "-",
                    mir::BinOp::Mul => "*",
                    mir::BinOp::Div => "/",
                    _ => unimplemented!(),
                };
                format!(
                    "({op} {} {})",
                    self.emit_operand(&operands.0),
                    self.emit_operand(&operands.1)
                )
            }
            mir::Rvalue::Aggregate(agg_kind, operands) => {
                let kind = match &**agg_kind {
                    mir::AggregateKind::Tuple => "tuple".to_string(),
                    mir::AggregateKind::Adt(def_id, variant_index, substs, _, _) => {
                        let adt_id = self.emit_def_path(*def_id);
                        let variant = variant_index.index();
                        let params = substs
                            .iter()
                            .map(|arg| self.emit_generic_arg(arg))
                            .intersperse(" ".to_string())
                            .collect::<String>();

                        format!("(adt {adt_id} {variant} [{params}])")
                    }
                    _ => unimplemented!("unknown aggregate kind"),
                };

                let ops = operands
                    .iter()
                    .map(|op| self.emit_operand(op))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!("({kind} [{ops}])")
            }
            _ => {
                eprintln!("unknown rvalue: {rvalue:?}");
                format!("unknown-rvalue")
            }
        }
    }

    fn emit_statement(&self, stmt: &mir::Statement<'tcx>) -> String {
        match &stmt.kind {
            mir::StatementKind::Assign(ass) => format!(
                "({} = {})",
                self.emit_place(&ass.0),
                self.emit_rvalue(&ass.1)
            ),
            mir::StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => format!(
                "(set-discriminant {} {})",
                self.emit_place(place),
                variant_index.index()
            ),
            mir::StatementKind::StorageLive(local) => format!("(storage-live _{})", local.index()),
            mir::StatementKind::StorageDead(local) => format!("(storage-dead _{})", local.index()),
            mir::StatementKind::Nop => format!("noop"),
            _ => {
                eprintln!("unknown stmt: {stmt:?}");
                "noop".to_string()
            }
        }
    }

    fn emit_terminator(&self, term: &mir::Terminator) -> String {
        match &term.kind {
            mir::TerminatorKind::Goto { target } => format!("(goto bb{})", target.index()),
            mir::TerminatorKind::Resume => "resume".to_string(),
            mir::TerminatorKind::Abort => "abort".to_string(),
            mir::TerminatorKind::Return => "return".to_string(),
            mir::TerminatorKind::Unreachable => "unreachable".to_string(),
            mir::TerminatorKind::Drop {
                place,
                target,
                unwind,
            } => {
                let targets = match unwind {
                    None => format!("(bb{})", target.index()),
                    Some(unwind_bb) => format!("(bb{} bb{})", target.index(), unwind_bb.index()),
                };
                format!("(drop {} {targets})", self.emit_place(place))
            }
            mir::TerminatorKind::DropAndReplace {
                place,
                value,
                target,
                unwind,
            } => {
                let targets = match unwind {
                    None => format!("(bb{})", target.index()),
                    Some(unwind_bb) => format!("(bb{} bb{})", target.index(), unwind_bb.index()),
                };
                format!(
                    "(drop-and-replace {} {} {targets})",
                    self.emit_place(place),
                    self.emit_operand(value)
                )
            }
            mir::TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                ..
            } => {
                let arg_ops = args
                    .iter()
                    .map(|arg| self.emit_operand(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!(
                    "(call {}[{arg_ops}] {} (bb{}))",
                    self.emit_operand(func),
                    self.emit_place(destination),
                    target.unwrap().index()
                )
            }
            _ => {
                eprintln!("unknown terminator: {term:?}");
                "abort".to_string()
            }
        }
    }

    pub fn emit_body(&self, body: &mir::Body<'tcx>) -> String {
        let mut body = body.clone();
        // In the MIR returned by the mir_built query, regions have been erased.
        // Thus, we have to assign fresh variables (?0, ?1, ...) to them here.
        let num_re_vars = renumber_mir::replace_regions_in_mir(&self.tcx, &mut body);

        let vars = (0..num_re_vars)
            .map(|var_idx| format!("(lifetime ?{var_idx})"))
            .intersperse("\n     ".to_string())
            .collect::<String>();

        let locals = body
            .local_decls
            .iter_enumerated()
            .map(|(local, decl)| {
                let mutability = match decl.mutability {
                    mir::Mutability::Not => "()",
                    mir::Mutability::Mut => "mut",
                };
                format!(
                    "(_{} {} {mutability})",
                    local.index(),
                    self.emit_ty(decl.ty)
                )
            })
            .intersperse("\n   ".to_string())
            .collect::<String>();

        // Basic blocks are labeled bb0, bb1, ...
        let blocks = body
            .basic_blocks()
            .iter_enumerated()
            .map(|(bb_id, bb_data)| {
                let stmts = bb_data
                    .statements
                    .iter()
                    .map(|stmt| self.emit_statement(stmt))
                    .intersperse("\n      ".to_string())
                    .collect::<String>();

                let term = self.emit_terminator(bb_data.terminator());
                format!(
                    "(bb{} {{\n     [{stmts}]\n     {term}\n   }})",
                    bb_id.index()
                )
            })
            .intersperse("\n   ".to_string())
            .collect::<String>();

        format!("(∃ [{vars}] {{\n  [{locals}]\n\n  [{blocks}]\n}})")
    }
}
