use crate::grammar::{
    expr::{Block, Expr, ExprData, PlaceExpr, PlaceExprData, Stmt},
    Crates, Fallible, RigidName, RigidTy, ScalarId, Ty, TyData, ValueId,
};
use formality_core::Upcast;
use libspecr::hidden::GcCow;
use libspecr::prelude::*;
use minirust_rs::lang;
use minirust_rs::mem::{PtrType, TupleHeadLayout};

mod seme_region;
use seme_region::SemeRegion;

mod test;

/// Counter for deterministic fresh name allocation.
#[derive(Clone)]
struct NameCounter(u32);

impl NameCounter {
    fn new() -> Self {
        NameCounter(0)
    }

    fn fresh(&mut self) -> libspecr::Name {
        let n = self.0;
        self.0 += 1;
        libspecr::Name::from_internal(n)
    }

    fn fresh_bb(&mut self) -> lang::BbName {
        lang::BbName(self.fresh())
    }

    fn fresh_local(&mut self) -> lang::LocalName {
        lang::LocalName(self.fresh())
    }
}

/// Codegen state threaded through judgments.
struct CodegenState<'a> {
    crates: &'a Crates,
    counter: NameCounter,
    /// Map from source variable name to (local name, Rust type).
    variables: Vec<(ValueId, lang::LocalName, Ty)>,
    /// Accumulated locals: local name → minirust type.
    locals: Vec<(lang::LocalName, lang::Type)>,
}

impl<'a> CodegenState<'a> {
    fn new(crates: &'a Crates) -> Self {
        CodegenState {
            crates,
            counter: NameCounter::new(),
            variables: Vec::new(),
            locals: Vec::new(),
        }
    }

    fn fresh_bb(&mut self) -> lang::BbName {
        self.counter.fresh_bb()
    }

    fn fresh_local(&mut self) -> lang::LocalName {
        self.counter.fresh_local()
    }

    fn alloc_local(&mut self, ty: lang::Type) -> lang::LocalName {
        let name = self.fresh_local();
        self.locals.push((name, ty));
        name
    }

    fn lookup_var(&self, id: &ValueId) -> Fallible<(lang::LocalName, Ty)> {
        self.variables
            .iter()
            .rev()
            .find(|(name, _, _)| name == id)
            .map(|(_, local, ty)| (*local, ty.clone()))
            .ok_or_else(|| anyhow::anyhow!("unbound variable `{id:?}`"))
    }
}

/// Convert a Rust type to a MiniRust type.
fn minirust_ty(ty: &Ty) -> Fallible<lang::Type> {
    match ty {
        Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
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
            RigidName::AdtId(_) => unimplemented!("adts"),
            RigidName::Never => Ok(unit_ty()),
            RigidName::Ref(_) | RigidName::Raw(_) => unimplemented!("refs and pointers"),
            RigidName::Tuple(arity) => {
                if *arity == 0 {
                    Ok(unit_ty())
                } else {
                    unimplemented!("non-unit tuples")
                }
            }
            RigidName::FnPtr(_) => unimplemented!("fnptrs"),
            RigidName::FnDef(_) => unimplemented!("fndefs"),
        },
        TyData::AliasTy(_) => unimplemented!("associated type normalization"),
        TyData::PredicateTy(_) => unimplemented!("predicate types"),
        TyData::Variable(v) => {
            anyhow::bail!("expected monomorphized input, found {v:?}")
        }
    }
}

/// The unit type: a zero-element tuple.
fn unit_ty() -> lang::Type {
    lang::Type::Tuple {
        sized_fields: list![],
        sized_head_layout: TupleHeadLayout {
            end: Size::ZERO,
            align: Align::ONE,
            packed_align: None,
        },
        unsized_field: GcCow::new(None),
    }
}

/// A unit value expression.
fn unit_value() -> lang::ValueExpr {
    lang::ValueExpr::Tuple(list![], unit_ty())
}

/// Build a scalar Ty from a ScalarId.
fn scalar_ty(s: ScalarId) -> Ty {
    RigidTy {
        name: RigidName::ScalarId(s),
        parameters: vec![],
    }
    .upcast()
}

/// Top-level entry point: compile a `Crates` to a MiniRust `Program`.
pub fn codegen_program(crates: &Crates) -> Fallible<lang::Program> {
    let mut state = CodegenState::new(crates);

    // Find main function
    let main_fn = crates.fn_named(&crate::rust::term("main"))?;
    let (_, fn_data) = main_fn.binder.open();

    // Codegen main's body
    let body_block = match &fn_data.body {
        crate::grammar::MaybeFnBody::FnBody(crate::grammar::FnBody::Expr(block)) => block,
        _ => anyhow::bail!("main must have an expression body"),
    };

    let ret_ty = minirust_ty(&fn_data.output_ty)?;
    let ret_local = state.alloc_local(ret_ty);

    let region = codegen_block(&mut state, body_block)?;

    // If the block has a fallthrough (didn't end with return), add implicit unit return
    let region = finalize_fn_body(region, ret_local);
    let main_entry = region.entry();

    let mut main_blocks = region.into_blocks();

    // Prepend StorageLive for all locals to the entry block
    let storage_lives: Vec<lang::Statement> = state
        .locals
        .iter()
        .map(|(name, _)| lang::Statement::StorageLive(*name))
        .collect();
    if let Some((_, entry_bb)) = main_blocks.first_mut() {
        let mut new_stmts: List<lang::Statement> = storage_lives.into_iter().collect();
        for s in entry_bb.statements {
            new_stmts.push(s);
        }
        entry_bb.statements = new_stmts;
    }

    let main_fn_name = lang::FnName(state.counter.fresh());
    let main_locals: Map<lang::LocalName, lang::Type> =
        state.locals.iter().map(|(k, v)| (*k, *v)).collect();
    let main_bb_map: Map<lang::BbName, lang::BasicBlock> = main_blocks.into_iter().collect();

    let main_function = lang::Function {
        locals: main_locals,
        args: list![],
        ret: ret_local,
        calling_convention: lang::CallingConvention::Rust,
        blocks: main_bb_map,
        start: main_entry,
    };

    // Build synthetic _start that calls main then exits
    let mut start_counter = NameCounter::new();
    let start_ret = lang::LocalName(start_counter.fresh());
    let start_main_ret = lang::LocalName(start_counter.fresh());

    let call_bb = lang::BbName(start_counter.fresh());
    let exit_bb = lang::BbName(start_counter.fresh());

    let call_block = lang::BasicBlock {
        statements: list![
            lang::Statement::StorageLive(start_ret),
            lang::Statement::StorageLive(start_main_ret)
        ],
        terminator: lang::Terminator::Call {
            callee: lang::ValueExpr::Constant(
                lang::Constant::FnPointer(main_fn_name),
                lang::Type::Ptr(PtrType::FnPtr),
            ),
            calling_convention: lang::CallingConvention::Rust,
            arguments: list![],
            ret: lang::PlaceExpr::Local(start_main_ret),
            next_block: Some(exit_bb),
            unwind_block: None,
        },
        kind: lang::BbKind::Regular,
    };

    let exit_block = lang::BasicBlock {
        statements: list![],
        terminator: lang::Terminator::Intrinsic {
            intrinsic: lang::IntrinsicOp::Exit,
            arguments: list![],
            ret: lang::PlaceExpr::Local(start_ret),
            next_block: None,
        },
        kind: lang::BbKind::Regular,
    };

    let start_fn_name = lang::FnName(start_counter.fresh());

    let mut start_locals = Map::new();
    start_locals.insert(start_ret, unit_ty());
    start_locals.insert(start_main_ret, ret_ty);

    let start_function = lang::Function {
        locals: start_locals,
        args: list![],
        ret: start_ret,
        calling_convention: lang::CallingConvention::C,
        blocks: {
            let mut m = Map::new();
            m.insert(call_bb, call_block);
            m.insert(exit_bb, exit_block);
            m
        },
        start: call_bb,
    };

    let mut functions = Map::new();
    functions.insert(main_fn_name, main_function);
    functions.insert(start_fn_name, start_function);

    Ok(lang::Program {
        functions,
        start: start_fn_name,
        globals: Map::new(),
        traits: Map::new(),
        vtables: Map::new(),
    })
}

/// Codegen a block (sequence of statements).
fn codegen_block(state: &mut CodegenState, block: &Block) -> Fallible<SemeRegion> {
    let mut region = SemeRegion::empty(state);
    for stmt in &block.stmts {
        let stmt_region = codegen_stmt(state, stmt)?;
        region = region.append(state, stmt_region);
    }
    Ok(region)
}

/// If the region still has a fallthrough, write unit to ret_local and emit Return.
fn finalize_fn_body(mut region: SemeRegion, ret_local: lang::LocalName) -> SemeRegion {
    if region.has_fallthrough() {
        region.push_stmt(lang::Statement::Assign {
            destination: lang::PlaceExpr::Local(ret_local),
            source: unit_value(),
        });
        region.terminate(lang::Terminator::Return);
    }
    region
}

/// Codegen a single statement.
fn codegen_stmt(state: &mut CodegenState, stmt: &Stmt) -> Fallible<SemeRegion> {
    match stmt {
        Stmt::Return { expr } => {
            // Evaluate the return expression into the return local.
            // The return local is always the first one allocated (index 0).
            let ret_local = lang::LocalName(libspecr::Name::from_internal(0));
            let mut region = codegen_expr_into(state, ret_local, expr)?;
            region.terminate(lang::Terminator::Return);
            Ok(region)
        }
        Stmt::Print { expr } => {
            // Evaluate expr into a temp, then emit PrintStdout intrinsic.
            let expr_ty = infer_expr_ty(expr)?;
            let mr_ty = minirust_ty(&expr_ty)?;
            let temp = state.alloc_local(mr_ty);
            let mut region = codegen_expr_into(state, temp, expr)?;

            let next_bb = state.fresh_bb();
            let print_ret = state.alloc_local(unit_ty());
            region.terminate(lang::Terminator::Intrinsic {
                intrinsic: lang::IntrinsicOp::PrintStdout,
                arguments: list![lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(temp)),
                }],
                ret: lang::PlaceExpr::Local(print_ret),
                next_block: Some(next_bb),
            });
            region.add_empty_block(next_bb);
            Ok(region)
        }
        _ => anyhow::bail!("codegen not yet implemented for this statement"),
    }
}

/// Codegen an expression, writing the result into `target`.
fn codegen_expr_into(
    state: &mut CodegenState,
    target: lang::LocalName,
    expr: &Expr,
) -> Fallible<SemeRegion> {
    match expr.data() {
        ExprData::Literal { value, ty } => {
            let mr_ty = minirust_ty(&scalar_ty(ty.clone()))?;
            let int_val = Int::from(*value);
            let source = lang::ValueExpr::Constant(lang::Constant::Int(int_val), mr_ty);
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source,
            });
            Ok(region)
        }
        ExprData::Place(place_expr) => {
            let source_place = codegen_place_expr(state, place_expr)?;
            let source = lang::ValueExpr::Load {
                source: GcCow::new(source_place),
            };
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source,
            });
            Ok(region)
        }
        ExprData::True | ExprData::False => {
            let val = matches!(expr.data(), ExprData::True);
            let source = lang::ValueExpr::Constant(lang::Constant::Bool(val), lang::Type::Bool);
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source,
            });
            Ok(region)
        }
        _ => anyhow::bail!("codegen not yet implemented for this expression"),
    }
}

/// Codegen a place expression.
fn codegen_place_expr(state: &CodegenState, place: &PlaceExpr) -> Fallible<lang::PlaceExpr> {
    match place.data() {
        PlaceExprData::Var(id) => {
            let (local, _ty) = state.lookup_var(id)?;
            Ok(lang::PlaceExpr::Local(local))
        }
        PlaceExprData::Parens(inner) => codegen_place_expr(state, inner),
        _ => anyhow::bail!("codegen not yet implemented for this place expression"),
    }
}

/// Infer the type of an expression (simple cases only).
fn infer_expr_ty(expr: &Expr) -> Fallible<Ty> {
    match expr.data() {
        ExprData::Literal { ty, .. } => Ok(scalar_ty(ty.clone())),
        ExprData::True | ExprData::False => Ok(Ty::bool()),
        _ => anyhow::bail!("cannot infer type of expression for print"),
    }
}
