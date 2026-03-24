#![expect(unused_variables)]
#![expect(dead_code)]

use formality_core::{Map, Set, Upcast};
use libspecr::{hidden::GcCow, prelude::Signedness};
use minirust_rs::lang;

use crate::grammar::{
    expr::{Block, Expr, PlaceExpr, Stmt, ValueId},
    Fallible, PredicateTy, Program, RigidName, ScalarId, Ty, TyData,
};

use super::{ExprData, PlaceExprData};

struct ExprBuilder<'b> {
    program: &'b Program,

    /// Map from source-level local variable IDs to minirust local variable names.
    variables: Map<ValueId, lang::LocalName>,

    /// List of minirust Local variables
    mr_locals: Vec<lang::Type>,

    /// List of minirust basic blocks
    mr_blocks: Vec<BasicBlockBuilder>,

    generated_locals: Set<String>,
}

struct BasicBlockBuilder {
    statements: Vec<lang::Statement>,
    terminator: Option<lang::Terminator>,
    kind: lang::BbKind,
}

struct Cursor {
    block_index: usize,
}

impl ExprBuilder<'_> {
    fn codegen_block(
        &mut self,
        mut cursor: Cursor,
        target: PlaceExpr,
        block: &Block,
    ) -> Fallible<Cursor> {
        let Block { label: _, stmts } = block;
        for stmt in stmts {
            cursor = self.codegen_stmt(cursor, stmt)?;
        }
        Ok(cursor)
    }

    fn codegen_stmt(&mut self, cursor: Cursor, stmt: &Stmt) -> Fallible<Cursor> {
        match stmt {
            Stmt::Let {
                label: _,
                id,
                ty,
                init,
            } => {
                let next_local = self.mr_locals.len();
                let mr_ty = self.minirust_ty(ty)?;
                self.mr_locals.push(mr_ty);
                self.variables
                    .insert(id.clone(), lang::LocalName(self.mr_name(next_local)));
                if let Some(init) = init {
                    self.codegen_expr_writing_to(cursor, id, &init.expr)
                } else {
                    Ok(cursor)
                }
            }

            Stmt::If { .. } => todo!(),
            Stmt::Expr { expr } => todo!(),
            Stmt::Loop { label, body } => todo!(),
            Stmt::Break { label } => todo!(),
            Stmt::Continue { label } => todo!(),
            Stmt::Return { expr } => todo!(),
            Stmt::Block(block) => todo!(),
            Stmt::Exists { binder } => todo!(),
        }
    }

    fn codegen_expr_writing_to(
        &mut self,
        mut cursor: Cursor,
        target: impl Upcast<PlaceExpr>,
        expr: &Expr,
    ) -> Fallible<Cursor> {
        let target: PlaceExpr = target.upcast();
        match expr.data() {
            ExprData::Assign { place, expr } => {
                cursor = self.codegen_expr_writing_to(cursor, place, expr)?;
                cursor = self.write_unit(cursor, target)?;
                Ok(cursor)
            }
            ExprData::Place(place_expr) => {
                let target = self.codegen_place_expr(&target)?;
                let source = self.codegen_place_expr(place_expr)?;
                let source = lang::ValueExpr::Load {
                    source: GcCow::new(source),
                };
                cursor = self.write_assign(cursor, target, source)?;
                Ok(cursor)
            }
            ExprData::Call { callee, args } => todo!(),
            ExprData::Literal { value, ty } => todo!(),
            ExprData::True | ExprData::False => todo!(),
            ExprData::Ref {
                kind,
                lt,
                place: expr,
            } => todo!(),
            ExprData::Struct {
                field_exprs,
                adt_id,
                turbofish,
            } => todo!(),
            ExprData::Turbofish { id, args } => todo!(),
        }
    }

    pub fn write_assign(
        &mut self,
        cursor: Cursor,
        target: lang::PlaceExpr,
        source: lang::ValueExpr,
    ) -> Fallible<Cursor> {
        let stmt = lang::Statement::Assign {
            destination: target,
            source,
        };
        self.mr_blocks[cursor.block_index].statements.push(stmt);
        Ok(cursor)
    }

    pub fn codegen_place_expr(&mut self, expr: &PlaceExpr) -> Fallible<lang::PlaceExpr> {
        match expr.data() {
            PlaceExprData::Var(id) => Ok(lang::PlaceExpr::Local(self.lookup_variable(id)?)),
            PlaceExprData::Parens(inner) => self.codegen_place_expr(inner),
            PlaceExprData::Deref { prefix: _ } => {
                // Ok(lang::PlaceExpr::Deref { operand: (), ty: () }}
                unimplemented!("deref expression")
            }
            PlaceExprData::Field {
                prefix: expr,
                field_name: field,
            } => {
                // Need to convert field into a field index
                unimplemented!("field projection")
            }
        }
    }

    fn write_unit(&mut self, cursor: Cursor, target: PlaceExpr) -> Fallible<Cursor> {
        Ok(cursor)
    }

    fn minirust_ty(&mut self, ty: &Ty) -> Fallible<lang::Type> {
        match ty.data() {
            TyData::RigidTy(rigid_ty) => match &rigid_ty.name {
                RigidName::AdtId(adt_id) => {
                    unimplemented!("adts")
                }
                RigidName::Never => {
                    unimplemented!("never")
                }
                RigidName::ScalarId(scalar_id) => {
                    let (signed, size) = match scalar_id {
                        ScalarId::U8 => (Signedness::Unsigned, 1),
                        ScalarId::U16 => (Signedness::Unsigned, 2),
                        ScalarId::U32 => (Signedness::Unsigned, 4),
                        ScalarId::U64 => (Signedness::Unsigned, 8),
                        ScalarId::I8 => (Signedness::Signed, 1),
                        ScalarId::I16 => (Signedness::Signed, 2),
                        ScalarId::I32 => (Signedness::Signed, 4),
                        ScalarId::I64 => (Signedness::Signed, 8),
                        ScalarId::Bool => return Ok(lang::Type::Bool),
                        ScalarId::Usize | ScalarId::Isize => {
                            unimplemented!("target dependent types")
                        }
                    };
                    Ok(lang::Type::Int(lang::IntType {
                        signed,
                        size: libspecr::Size::from_bytes_const(size),
                    }))
                }
                RigidName::Ref(ref_kind) => unimplemented!("refs and pointers"),
                RigidName::Tuple(_) => unimplemented!("tuples"),
                RigidName::FnPtr(_) => unimplemented!("fnptrs"),
                RigidName::FnDef(_) => unimplemented!("fndefs"),
            },
            TyData::AliasTy(_alias_ty) => {
                unimplemented!("associated type normalization")
            }
            TyData::PredicateTy(predicate_ty) => match predicate_ty {
                PredicateTy::ForAll(core_binder) => {
                    unimplemented!("forall types")
                }
            },
            TyData::Variable(core_variable) => {
                anyhow::bail!("expected monomorphized input, found {core_variable:?}")
            }
        }
    }

    fn start_block(&mut self, kind: lang::BbKind) -> Cursor {
        let block_index = self.mr_blocks.len();
        self.mr_blocks.push(BasicBlockBuilder {
            statements: vec![],
            terminator: None,
            kind,
        });
        Cursor { block_index }
    }

    fn lookup_variable(&self, id: &ValueId) -> Fallible<lang::LocalName> {
        self.variables
            .get(id)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("unbound variable {id:?}"))
    }

    fn mr_name(&self, index: usize) -> libspecr::Name {
        let index = index as u32;
        libspecr::Name::from_internal(index)
    }
}
