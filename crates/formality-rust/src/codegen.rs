use crate::grammar::{
    expr::{Block, Expr, ExprData, PlaceExpr, PlaceExprData, Stmt},
    Crates, Fallible, Parameter, RigidName, RigidTy, ScalarId, Ty, TyData, ValueId,
};
use formality_core::Upcast;
use libspecr::hidden::GcCow;
use libspecr::prelude::*;
use minirust_rs::lang;
use minirust_rs::mem::{PtrType, TupleHeadLayout};

mod seme_region;
use seme_region::SemeRegion;

mod test;

// ---------------------------------------------------------------------------
// Name allocation
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Monomorphization
// ---------------------------------------------------------------------------

/// A monomorphization key: function id + type parameters.
#[derive(Clone, PartialEq, Eq)]
struct MonoKey {
    id: ValueId,
    args: Vec<Parameter>,
}

/// Global codegen context that accumulates functions across the worklist.
struct ProgramBuilder<'a> {
    crates: &'a Crates,
    /// Global counter for FnName allocation.
    fn_counter: NameCounter,
    /// Worklist of functions to codegen.
    worklist: Vec<(MonoKey, lang::FnName)>,
    /// Already-processed mono keys → FnName.
    processed: Vec<(MonoKey, lang::FnName)>,
    /// Accumulated functions.
    functions: Map<lang::FnName, lang::Function>,
}

impl<'a> ProgramBuilder<'a> {
    fn new(crates: &'a Crates) -> Self {
        ProgramBuilder {
            crates,
            fn_counter: NameCounter::new(),
            worklist: Vec::new(),
            processed: Vec::new(),
            functions: Map::new(),
        }
    }

    /// Ensure a function is in the worklist or already processed. Returns its FnName.
    fn ensure_function(&mut self, key: MonoKey) -> lang::FnName {
        // Check if already processed or in worklist
        for (k, name) in self.processed.iter().chain(self.worklist.iter()) {
            if *k == key {
                return *name;
            }
        }
        let name = lang::FnName(self.fn_counter.fresh());
        self.worklist.push((key, name));
        name
    }

    /// Process the worklist until empty.
    fn process_worklist(&mut self) -> Fallible<()> {
        while let Some((key, fn_name)) = self.worklist.pop() {
            self.processed.push((key.clone(), fn_name));
            let function = codegen_function(self, &key)?;
            self.functions.insert(fn_name, function);
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Per-function codegen state
// ---------------------------------------------------------------------------

struct CodegenState<'a, 'b> {
    builder: &'a mut ProgramBuilder<'b>,
    counter: NameCounter,
    variables: Vec<(ValueId, lang::LocalName, Ty)>,
    locals: Vec<(lang::LocalName, lang::Type)>,
    ret_local: lang::LocalName,
    /// Scope stack for break/continue targets.
    loop_scopes: Vec<LoopScope>,
}

/// A loop scope for break/continue resolution.
struct LoopScope {
    label: crate::grammar::expr::LabelId,
    loop_start: lang::BbName,
    exit_block: lang::BbName,
}

impl<'a, 'b> CodegenState<'a, 'b> {
    fn fresh_bb(&mut self) -> lang::BbName {
        self.counter.fresh_bb()
    }
    fn alloc_local(&mut self, ty: lang::Type) -> lang::LocalName {
        let name = self.counter.fresh_local();
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
    fn lookup_loop(
        &self,
        label: &crate::grammar::expr::LabelId,
    ) -> Fallible<(lang::BbName, lang::BbName)> {
        self.loop_scopes
            .iter()
            .rev()
            .find(|s| s.label == *label)
            .map(|s| (s.loop_start, s.exit_block))
            .ok_or_else(|| anyhow::anyhow!("no loop with label `{label:?}`"))
    }
}

// ---------------------------------------------------------------------------
// Type helpers
// ---------------------------------------------------------------------------

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
            RigidName::FnDef(_) => Ok(unit_ty()), // FnDef is a ZST
        },
        TyData::AliasTy(_) => unimplemented!("associated type normalization"),
        TyData::PredicateTy(_) => unimplemented!("predicate types"),
        TyData::Variable(v) => {
            anyhow::bail!("expected monomorphized input, found {v:?}")
        }
    }
}

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

fn unit_value() -> lang::ValueExpr {
    lang::ValueExpr::Tuple(list![], unit_ty())
}

fn scalar_ty(s: ScalarId) -> Ty {
    RigidTy {
        name: RigidName::ScalarId(s),
        parameters: vec![],
    }
    .upcast()
}

// ---------------------------------------------------------------------------
// Top-level entry point
// ---------------------------------------------------------------------------

pub fn codegen_program(crates: &Crates) -> Fallible<lang::Program> {
    let mut builder = ProgramBuilder::new(crates);

    // Enqueue main
    let main_key = MonoKey {
        id: crate::rust::term("main"),
        args: vec![],
    };
    let main_fn_name = builder.ensure_function(main_key);

    // Process all functions
    builder.process_worklist()?;

    // Build synthetic _start
    let mut sc = NameCounter::new();
    let start_ret = lang::LocalName(sc.fresh());
    let start_main_ret = lang::LocalName(sc.fresh());
    let call_bb = lang::BbName(sc.fresh());
    let exit_bb = lang::BbName(sc.fresh());

    let main_ret_ty = builder
        .functions
        .get(main_fn_name)
        .map(|f| f.locals.get(f.ret).unwrap())
        .unwrap();

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

    let start_fn_name = lang::FnName(sc.fresh());
    let mut start_locals = Map::new();
    start_locals.insert(start_ret, unit_ty());
    start_locals.insert(start_main_ret, main_ret_ty);

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

    builder.functions.insert(start_fn_name, start_function);

    Ok(lang::Program {
        functions: builder.functions,
        start: start_fn_name,
        globals: Map::new(),
        traits: Map::new(),
        vtables: Map::new(),
    })
}

// ---------------------------------------------------------------------------
// Function codegen
// ---------------------------------------------------------------------------

fn codegen_function(builder: &mut ProgramBuilder, key: &MonoKey) -> Fallible<lang::Function> {
    let fn_def = builder.crates.fn_named(&key.id)?;
    let fn_data = if key.args.is_empty() {
        let (_, data) = fn_def.binder.open();
        data
    } else {
        fn_def.binder.instantiate_with(&key.args)?
    };

    let body_block = match &fn_data.body {
        crate::grammar::MaybeFnBody::FnBody(crate::grammar::FnBody::Expr(block)) => block,
        _ => anyhow::bail!("function {:?} must have an expression body", key.id),
    };

    let ret_ty = minirust_ty(&fn_data.output_ty)?;
    let mut counter = NameCounter::new();
    let ret_local = lang::LocalName(counter.fresh_local().0);
    let mut locals = vec![(ret_local, ret_ty)];

    // Allocate arg locals
    let mut arg_locals = list![];
    let mut variables = Vec::new();
    for arg in &fn_data.input_args {
        let mr_ty = minirust_ty(&arg.ty)?;
        let local = lang::LocalName(counter.fresh_local().0);
        locals.push((local, mr_ty));
        arg_locals.push(local);
        variables.push((arg.id.clone(), local, arg.ty.clone()));
    }

    let mut state = CodegenState {
        builder,
        counter,
        variables,
        locals,
        ret_local,
        loop_scopes: Vec::new(),
    };

    let region = codegen_block(&mut state, body_block)?;
    let region = finalize_fn_body(region, state.ret_local);
    let entry = region.entry();
    let mut blocks = region.into_blocks();

    // Prepend StorageLive for non-arg, non-ret locals
    let mut skip_locals = vec![state.ret_local];
    for al in arg_locals {
        skip_locals.push(al);
    }
    let storage_lives: Vec<lang::Statement> = state
        .locals
        .iter()
        .filter(|(name, _)| !skip_locals.contains(name))
        .map(|(name, _)| lang::Statement::StorageLive(*name))
        .collect();
    if let Some((_, entry_bb)) = blocks.first_mut() {
        let mut new_stmts: List<lang::Statement> = storage_lives.into_iter().collect();
        for s in entry_bb.statements {
            new_stmts.push(s);
        }
        entry_bb.statements = new_stmts;
    }

    let locals_map: Map<lang::LocalName, lang::Type> =
        state.locals.iter().map(|(k, v)| (*k, *v)).collect();
    let blocks_map: Map<lang::BbName, lang::BasicBlock> = blocks.into_iter().collect();

    Ok(lang::Function {
        locals: locals_map,
        args: arg_locals,
        ret: state.ret_local,
        calling_convention: lang::CallingConvention::Rust,
        blocks: blocks_map,
        start: entry,
    })
}

// ---------------------------------------------------------------------------
// Block / statement / expression codegen
// ---------------------------------------------------------------------------

fn codegen_block(state: &mut CodegenState, block: &Block) -> Fallible<SemeRegion> {
    let mut region = SemeRegion::empty(state);
    for stmt in &block.stmts {
        let stmt_region = codegen_stmt(state, stmt)?;
        region = region.append(state, stmt_region);
    }
    Ok(region)
}

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

fn codegen_stmt(state: &mut CodegenState, stmt: &Stmt) -> Fallible<SemeRegion> {
    match stmt {
        Stmt::Let {
            label: _,
            id,
            ty,
            init,
        } => {
            let mr_ty = minirust_ty(ty)?;
            let local = state.alloc_local(mr_ty);
            state.variables.push((id.clone(), local, ty.clone()));
            if let Some(init) = init {
                codegen_expr_into(state, local, &init.expr)
            } else {
                Ok(SemeRegion::empty(state))
            }
        }
        Stmt::Return { expr } => {
            let ret_local = state.ret_local;
            let mut region = codegen_expr_into(state, ret_local, expr)?;
            region.terminate(lang::Terminator::Return);
            Ok(region)
        }
        Stmt::Print { expr } => {
            let expr_ty = infer_expr_ty(state, expr)?;
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
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            // Evaluate condition into a bool temp
            let cond_temp = state.alloc_local(lang::Type::Bool);
            let cond_region = codegen_expr_into(state, cond_temp, condition)?;

            // Codegen both branches
            let then_region = codegen_block(state, then_block)?;
            let else_region = codegen_block(state, else_block)?;

            // Compose with branch
            Ok(cond_region.branch_on_bool(state, cond_temp, then_region, else_region))
        }
        Stmt::Expr { expr } => {
            let expr_ty = infer_expr_ty(state, expr)?;
            let mr_ty = minirust_ty(&expr_ty)?;
            let temp = state.alloc_local(mr_ty);
            codegen_expr_into(state, temp, expr)
        }
        Stmt::Loop { label, body } => {
            let label = label
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("loop must have a label for codegen"))?;

            let loop_start = state.fresh_bb();
            let exit_block = state.fresh_bb();

            // Push loop scope
            state.loop_scopes.push(LoopScope {
                label: label.id.clone(),
                loop_start,
                exit_block,
            });

            // Codegen the body
            let body_region = codegen_block(state, body)?;

            // Pop loop scope
            state.loop_scopes.pop();

            // Build the loop structure:
            // entry → Goto(loop_start) → body → Goto(loop_start)
            //                                 → break → exit_block
            let mut region = SemeRegion::empty(state);

            // Terminate entry with Goto(loop_start)
            region.terminate(lang::Terminator::Goto(loop_start));

            // Add loop_start as a new block, inline body into it
            region.add_empty_block(loop_start);

            // Append body
            region = region.append(state, body_region);

            // If body has fallthrough, add back-edge to loop_start
            if region.has_fallthrough() {
                region.terminate(lang::Terminator::Goto(loop_start));
            }

            // The exit block is the new fallthrough
            region.add_empty_block(exit_block);
            Ok(region)
        }
        Stmt::Break { label } => {
            let (_, exit_block) = state.lookup_loop(label)?;
            let mut region = SemeRegion::empty(state);
            region.terminate(lang::Terminator::Goto(exit_block));
            Ok(region)
        }
        Stmt::Continue { label } => {
            let (loop_start, _) = state.lookup_loop(label)?;
            let mut region = SemeRegion::empty(state);
            region.terminate(lang::Terminator::Goto(loop_start));
            Ok(region)
        }
        _ => anyhow::bail!("codegen not yet implemented for this statement"),
    }
}

fn codegen_expr_into(
    state: &mut CodegenState,
    target: lang::LocalName,
    expr: &Expr,
) -> Fallible<SemeRegion> {
    match expr.data() {
        ExprData::Literal { value, ty } => {
            let mr_ty = minirust_ty(&scalar_ty(ty.clone()))?;
            let source = lang::ValueExpr::Constant(lang::Constant::Int(Int::from(*value)), mr_ty);
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source,
            });
            Ok(region)
        }
        ExprData::Place(place_expr) => {
            let source_place = codegen_place_expr(state, place_expr)?;
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: lang::ValueExpr::Load {
                    source: GcCow::new(source_place),
                },
            });
            Ok(region)
        }
        ExprData::True | ExprData::False => {
            let val = matches!(expr.data(), ExprData::True);
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: lang::ValueExpr::Constant(lang::Constant::Bool(val), lang::Type::Bool),
            });
            Ok(region)
        }
        ExprData::Assign { place, expr } => {
            let dest_place = codegen_place_expr(state, place)?;
            let rhs_ty = infer_expr_ty(state, expr)?;
            let mr_ty = minirust_ty(&rhs_ty)?;
            let rhs_temp = state.alloc_local(mr_ty);
            let mut region = codegen_expr_into(state, rhs_temp, expr)?;
            region.push_stmt(lang::Statement::Assign {
                destination: dest_place,
                source: lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(rhs_temp)),
                },
            });
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: unit_value(),
            });
            Ok(region)
        }
        ExprData::Turbofish { id, args } => {
            // Turbofish produces a ZST (FnDef type). The value is meaningless.
            // Ensure the function is in the worklist.
            let key = MonoKey {
                id: id.clone(),
                args: args.clone(),
            };
            let _fn_name = state.builder.ensure_function(key);
            // Write unit to target (FnDef is a ZST)
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: unit_value(),
            });
            // Register the Rust type as FnDef for later call resolution
            // We track this via the variable system — the Let that binds this
            // will register the variable with its declared type.
            Ok(region)
        }
        ExprData::Call { callee, args } => {
            // Resolve the function to call from the callee expression
            let fn_name = resolve_call_from_expr(state, callee)?;

            // Evaluate arguments into temps
            let mut region = SemeRegion::empty(state);
            let mut arg_exprs = list![];
            for arg in args {
                let arg_ty = infer_expr_ty(state, arg)?;
                let mr_ty = minirust_ty(&arg_ty)?;
                let arg_temp = state.alloc_local(mr_ty);
                let arg_region = codegen_expr_into(state, arg_temp, arg)?;
                region = region.append(state, arg_region);
                arg_exprs.push(lang::ArgumentExpr::ByValue(lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(arg_temp)),
                }));
            }

            // Emit Call terminator
            let next_bb = state.fresh_bb();
            region.terminate(lang::Terminator::Call {
                callee: lang::ValueExpr::Constant(
                    lang::Constant::FnPointer(fn_name),
                    lang::Type::Ptr(PtrType::FnPtr),
                ),
                calling_convention: lang::CallingConvention::Rust,
                arguments: arg_exprs,
                ret: lang::PlaceExpr::Local(target),
                next_block: Some(next_bb),
                unwind_block: None,
            });
            region.add_empty_block(next_bb);
            Ok(region)
        }
        _ => anyhow::bail!("codegen not yet implemented for this expression"),
    }
}

/// Resolve a call target from the callee expression.
fn resolve_call_from_expr(state: &mut CodegenState, callee: &Expr) -> Fallible<lang::FnName> {
    match callee.data() {
        ExprData::Turbofish { id, args } => {
            let key = MonoKey {
                id: id.clone(),
                args: args.clone(),
            };
            Ok(state.builder.ensure_function(key))
        }
        ExprData::Place(place) => {
            // Variable holding a FnDef — look up its Rust type
            let ty = infer_place_ty(state, place)?;
            match &ty {
                Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
                    RigidName::FnDef(id) => {
                        let key = MonoKey {
                            id: id.clone(),
                            args: vec![],
                        };
                        Ok(state.builder.ensure_function(key))
                    }
                    _ => anyhow::bail!("cannot call non-function type: {ty:?}"),
                },
                _ => anyhow::bail!("cannot call non-function type: {ty:?}"),
            }
        }
        _ => anyhow::bail!("unsupported callee expression"),
    }
}

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

// ---------------------------------------------------------------------------
// Type inference (for codegen)
// ---------------------------------------------------------------------------

fn infer_expr_ty(state: &CodegenState, expr: &Expr) -> Fallible<Ty> {
    match expr.data() {
        ExprData::Literal { ty, .. } => Ok(scalar_ty(ty.clone())),
        ExprData::True | ExprData::False => Ok(Ty::bool()),
        ExprData::Place(place) => infer_place_ty(state, place),
        ExprData::Assign { .. } => Ok(Ty::unit()),
        ExprData::Turbofish { id, args: _ } => {
            // Turbofish produces a FnDef type
            Ok(Ty::rigid(RigidName::FnDef(id.clone()), ()))
        }
        ExprData::Call { callee, .. } => {
            // The return type of a call is the return type of the callee function
            let callee_ty = infer_expr_ty(state, callee)?;
            infer_call_return_ty(state, &callee_ty)
        }
        _ => anyhow::bail!("cannot infer type of expression"),
    }
}

fn infer_call_return_ty(state: &CodegenState, callee_ty: &Ty) -> Fallible<Ty> {
    match callee_ty {
        Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
            RigidName::FnDef(id) => {
                let fn_def = state.builder.crates.fn_named(id)?;
                let (_, fn_data) = fn_def.binder.open();
                Ok(fn_data.output_ty.clone())
            }
            _ => anyhow::bail!("cannot infer return type of non-function: {callee_ty:?}"),
        },
        _ => anyhow::bail!("cannot infer return type of non-function: {callee_ty:?}"),
    }
}

fn infer_place_ty(state: &CodegenState, place: &PlaceExpr) -> Fallible<Ty> {
    match place.data() {
        PlaceExprData::Var(id) => {
            let (_, ty) = state.lookup_var(id)?;
            Ok(ty)
        }
        PlaceExprData::Parens(inner) => infer_place_ty(state, inner),
        _ => anyhow::bail!("cannot infer type of place expression"),
    }
}
