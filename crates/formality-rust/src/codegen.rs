use crate::grammar::{
    expr::{Block, Expr, ExprData, PlaceExpr, PlaceExprData, Stmt},
    Crates, Fallible, Lt, Parameter, ParameterKind, RigidName, RigidTy, ScalarId, Ty, TyData,
    ValueId,
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
    fn minirust_ty(&self, ty: &Ty) -> Fallible<lang::Type> {
        minirust_ty(self.builder.crates, ty)
    }
}

// ---------------------------------------------------------------------------
// Type helpers
// ---------------------------------------------------------------------------

fn minirust_ty(crates: &Crates, ty: &Ty) -> Fallible<lang::Type> {
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
                    ScalarId::Usize => (Signedness::Unsigned, 8),
                    ScalarId::Isize => (Signedness::Signed, 8),
                };
                Ok(lang::Type::Int(lang::IntType {
                    signed,
                    size: libspecr::Size::from_bytes_const(size),
                }))
            }
            RigidName::AdtId(adt_id) => {
                // Look up struct, compute layout
                // For now, use declaration order (no randomization)
                let parameters = &rigid_ty.parameters;
                struct_minirust_ty(crates, adt_id, parameters)
            }
            RigidName::Never => Ok(unit_ty()),
            RigidName::Ref(ref_kind) => {
                // Ref type: Type::Ptr with PtrType::Ref
                // Parameters: [Lt, pointee_ty]
                let pointee_ty = match rigid_ty.parameters.get(1) {
                    Some(Parameter::Ty(ty)) => ty.as_ref(),
                    _ => anyhow::bail!("ref type missing pointee parameter"),
                };
                let pointee_mr_ty = minirust_ty(crates, pointee_ty)?;
                let (pointee_size, pointee_align) = type_size_align(&pointee_mr_ty);
                let mutbl = match ref_kind {
                    crate::grammar::RefKind::Shared => Mutability::Immutable,
                    crate::grammar::RefKind::Mut => Mutability::Mutable,
                };
                Ok(lang::Type::Ptr(PtrType::Ref {
                    mutbl,
                    pointee: minirust_rs::mem::PointeeInfo {
                        layout: minirust_rs::mem::LayoutStrategy::Sized(
                            pointee_size,
                            pointee_align,
                        ),
                        inhabited: true,
                        freeze: true,
                        unpin: true,
                        unsafe_cells: minirust_rs::mem::UnsafeCellStrategy::Sized {
                            cells: list![],
                        },
                    },
                }))
            }
            RigidName::Raw(_) => unimplemented!("raw pointers"),
            RigidName::Tuple(arity) => {
                if *arity == 0 {
                    Ok(unit_ty())
                } else {
                    // Non-unit tuple: lay out positional fields
                    let mut offset = Size::ZERO;
                    let mut max_align = Align::ONE;
                    let mut sized_fields: Vec<(Size, lang::Type)> = Vec::new();

                    for param in &rigid_ty.parameters {
                        if let Parameter::Ty(ty) = param {
                            let field_ty = minirust_ty(crates, ty)?;
                            let (field_size, field_align) = type_size_align(&field_ty);
                            let align_bytes = field_align.bytes();
                            let offset_bytes = offset.bytes();
                            let aligned =
                                (offset_bytes + align_bytes - 1) / align_bytes * align_bytes;
                            offset = Size::from_bytes(aligned).unwrap();
                            sized_fields.push((offset, field_ty));
                            offset = Size::from_bytes(aligned + field_size.bytes()).unwrap();
                            if field_align.bytes() > max_align.bytes() {
                                max_align = field_align;
                            }
                        }
                    }

                    let total_bytes = offset.bytes();
                    let align_bytes = max_align.bytes();
                    let padded = (total_bytes + align_bytes - 1) / align_bytes * align_bytes;
                    let total_size = Size::from_bytes(padded).unwrap();

                    Ok(lang::Type::Tuple {
                        sized_fields: sized_fields.into_iter().collect(),
                        sized_head_layout: TupleHeadLayout {
                            end: total_size,
                            align: max_align,
                            packed_align: None,
                        },
                        unsized_field: GcCow::new(None),
                    })
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

/// Compute the MiniRust type for a struct.
fn struct_minirust_ty(
    crates: &Crates,
    adt_id: &crate::grammar::AdtId,
    parameters: &[Parameter],
) -> Fallible<lang::Type> {
    let s = crates.struct_named(adt_id)?;
    let bound_data = if parameters.is_empty() {
        let (_, data) = s.binder.open();
        data
    } else {
        s.binder.instantiate_with(parameters)?
    };

    // Lay out fields in declaration order (no randomization for now)
    let mut offset = Size::ZERO;
    let mut max_align = Align::ONE;
    let mut sized_fields: Vec<(Size, lang::Type)> = Vec::new();

    for field in &bound_data.fields {
        let field_ty = minirust_ty(crates, &field.ty)?;
        let (field_size, field_align) = type_size_align(&field_ty);

        // Align offset
        let align_bytes = field_align.bytes();
        let offset_bytes = offset.bytes();
        let aligned = (offset_bytes + align_bytes - 1) / align_bytes * align_bytes;
        offset = Size::from_bytes(aligned).unwrap();

        sized_fields.push((offset, field_ty));

        offset = Size::from_bytes(aligned + field_size.bytes()).unwrap();
        if field_align.bytes() > max_align.bytes() {
            max_align = field_align;
        }
    }

    // Round up total size to alignment
    let total_bytes = offset.bytes();
    let align_bytes = max_align.bytes();
    let padded = (total_bytes + align_bytes - 1) / align_bytes * align_bytes;
    let total_size = Size::from_bytes(padded).unwrap();

    Ok(lang::Type::Tuple {
        sized_fields: sized_fields.into_iter().collect(),
        sized_head_layout: TupleHeadLayout {
            end: total_size,
            align: max_align,
            packed_align: None,
        },
        unsized_field: GcCow::new(None),
    })
}

/// Get the size and alignment of a MiniRust type.
fn type_size_align(ty: &lang::Type) -> (Size, Align) {
    match ty {
        lang::Type::Int(int_ty) => {
            let size = int_ty.size;
            // Alignment = min(size, 8) for natural alignment
            let align = size.min(Size::from_bytes_const(8));
            (size, Align::from_bytes(align.bytes()).unwrap())
        }
        lang::Type::Bool => (Size::from_bytes_const(1), Align::ONE),
        lang::Type::Tuple {
            sized_head_layout, ..
        } => (sized_head_layout.end, sized_head_layout.align),
        lang::Type::Ptr(_) => (Size::from_bytes_const(8), Align::from_bytes(8).unwrap()),
        _ => unimplemented!("type_size_align for {:?}", ty),
    }
}

/// Get the field index for a field name in a struct.
fn struct_field_index(
    crates: &Crates,
    adt_id: &crate::grammar::AdtId,
    parameters: &[Parameter],
    field_name: &crate::grammar::FieldName,
) -> Fallible<(usize, Ty)> {
    let s = crates.struct_named(adt_id)?;
    let bound_data = if parameters.is_empty() {
        let (_, data) = s.binder.open();
        data
    } else {
        s.binder.instantiate_with(parameters)?
    };

    for (i, field) in bound_data.fields.iter().enumerate() {
        if field.name == *field_name {
            return Ok((i, field.ty.clone()));
        }
    }
    anyhow::bail!("no field {:?} in struct {:?}", field_name, adt_id)
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

    let ret_ty = minirust_ty(builder.crates, &fn_data.output_ty)?;
    let mut counter = NameCounter::new();
    let ret_local = lang::LocalName(counter.fresh_local().0);
    let mut locals = vec![(ret_local, ret_ty)];

    // Allocate arg locals
    let mut arg_locals = list![];
    let mut variables = Vec::new();
    for arg in &fn_data.input_args {
        let mr_ty = minirust_ty(builder.crates, &arg.ty)?;
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
            let mr_ty = state.minirust_ty(ty)?;
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
            let mr_ty = state.minirust_ty(&expr_ty)?;
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
            let mr_ty = state.minirust_ty(&expr_ty)?;
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
        Stmt::Block(block) => codegen_block(state, block),
        Stmt::Exists { binder } => {
            // Instantiate lifetime params with Lt::Erased; fail on type/const params
            let params: Vec<Parameter> = binder
                .kinds()
                .iter()
                .map(|kind| match kind {
                    ParameterKind::Lt => Ok(Lt::Erased.upcast()),
                    ParameterKind::Ty => {
                        anyhow::bail!("exists block with type parameter in codegen")
                    }
                    ParameterKind::Const => {
                        anyhow::bail!("exists block with const parameter in codegen")
                    }
                })
                .collect::<Fallible<_>>()?;
            let block = binder.instantiate_with(&params)?;
            codegen_block(state, &block)
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
            let mr_ty = state.minirust_ty(&scalar_ty(ty.clone()))?;
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
            let mr_ty = state.minirust_ty(&rhs_ty)?;
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
                let mr_ty = state.minirust_ty(&arg_ty)?;
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
        ExprData::Ref { kind, lt: _, place } => {
            let place_expr = codegen_place_expr(state, place)?;
            let ref_kind = match kind {
                crate::grammar::RefKind::Shared => Mutability::Immutable,
                crate::grammar::RefKind::Mut => Mutability::Mutable,
            };
            // Get pointee type for PtrType
            let pointee_ty = infer_place_ty(state, place)?;
            let pointee_mr_ty = state.minirust_ty(&pointee_ty)?;
            let (pointee_size, pointee_align) = type_size_align(&pointee_mr_ty);
            let ptr_ty = PtrType::Ref {
                mutbl: ref_kind,
                pointee: minirust_rs::mem::PointeeInfo {
                    layout: minirust_rs::mem::LayoutStrategy::Sized(pointee_size, pointee_align),
                    inhabited: true,
                    freeze: true,
                    unpin: true,
                    unsafe_cells: minirust_rs::mem::UnsafeCellStrategy::Sized { cells: list![] },
                },
            };
            let source = lang::ValueExpr::AddrOf {
                target: GcCow::new(place_expr),
                ptr_ty,
            };
            let mut region = SemeRegion::empty(state);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source,
            });
            Ok(region)
        }
        ExprData::Struct {
            field_exprs,
            adt_id,
            turbofish,
        } => {
            // Evaluate each field expression into a temp
            let mut region = SemeRegion::empty(state);
            let mut field_values = Vec::new();

            // Get the struct definition to know field order
            let s = state.builder.crates.struct_named(adt_id)?;
            let bound_data = if turbofish.parameters.is_empty() {
                let (_, data) = s.binder.open();
                data
            } else {
                s.binder.instantiate_with(&turbofish.parameters)?
            };

            // For each field in declaration order, find the matching field_expr
            for field_def in &bound_data.fields {
                let field_expr = field_exprs
                    .iter()
                    .find(|fe| fe.name == field_def.name)
                    .ok_or_else(|| {
                        anyhow::anyhow!("missing field {:?} in struct literal", field_def.name)
                    })?;
                let mr_ty = state.minirust_ty(&field_def.ty)?;
                let temp = state.alloc_local(mr_ty);
                let expr_region = codegen_expr_into(state, temp, &field_expr.value)?;
                region = region.append(state, expr_region);
                field_values.push(lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(temp)),
                });
            }

            // Build the tuple value
            let struct_ty = state.minirust_ty(&Ty::rigid(
                RigidName::AdtId(adt_id.clone()),
                turbofish.parameters.clone(),
            ))?;
            let tuple_val = lang::ValueExpr::Tuple(field_values.into_iter().collect(), struct_ty);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: tuple_val,
            });
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
        PlaceExprData::Deref { prefix } => {
            let prefix_place = codegen_place_expr(state, prefix)?;
            let prefix_ty = infer_place_ty(state, prefix)?;
            // Get the pointee type
            let pointee_ty = match &prefix_ty {
                Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
                    RigidName::Ref(_) => match rigid_ty.parameters.get(1) {
                        Some(Parameter::Ty(ty)) => ty.as_ref().clone(),
                        _ => anyhow::bail!("ref type missing pointee parameter"),
                    },
                    _ => anyhow::bail!("deref on non-reference type"),
                },
                _ => anyhow::bail!("deref on non-reference type"),
            };
            let pointee_mr_ty = state.minirust_ty(&pointee_ty)?;
            Ok(lang::PlaceExpr::Deref {
                operand: GcCow::new(lang::ValueExpr::Load {
                    source: GcCow::new(prefix_place),
                }),
                ty: pointee_mr_ty,
            })
        }
        PlaceExprData::Field { prefix, field_name } => {
            let prefix_place = codegen_place_expr(state, prefix)?;
            let prefix_ty = infer_place_ty(state, prefix)?;

            // Resolve field index from the Rust type
            match &prefix_ty {
                Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
                    RigidName::AdtId(adt_id) => {
                        let (idx, _field_ty) = struct_field_index(
                            state.builder.crates,
                            adt_id,
                            &rigid_ty.parameters,
                            field_name,
                        )?;
                        Ok(lang::PlaceExpr::Field {
                            root: GcCow::new(prefix_place),
                            field: Int::from(idx),
                        })
                    }
                    _ => anyhow::bail!("field access on non-struct type: {prefix_ty:?}"),
                },
                _ => anyhow::bail!("field access on non-struct type: {prefix_ty:?}"),
            }
        }
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
        ExprData::Ref {
            kind, lt, place, ..
        } => {
            let pointee_ty = infer_place_ty(state, place)?;
            Ok(pointee_ty.ref_ty_of_kind(kind, lt.clone()))
        }
        ExprData::Assign { .. } => Ok(Ty::unit()),
        ExprData::Struct {
            adt_id, turbofish, ..
        } => Ok(Ty::rigid(
            RigidName::AdtId(adt_id.clone()),
            turbofish.parameters.clone(),
        )),
        ExprData::Turbofish { id, args: _ } => {
            // Turbofish produces a FnDef type
            Ok(Ty::rigid(RigidName::FnDef(id.clone()), ()))
        }
        ExprData::Call { callee, .. } => {
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
        PlaceExprData::Var(id) => match state.lookup_var(id) {
            Ok((_, ty)) => Ok(ty),
            Err(_) if state.builder.crates.fn_named(id).is_ok() => {
                Ok(Ty::rigid(RigidName::FnDef(id.clone()), ()))
            }
            Err(e) => Err(e),
        },
        PlaceExprData::Parens(inner) => infer_place_ty(state, inner),
        PlaceExprData::Deref { prefix } => {
            let prefix_ty = infer_place_ty(state, prefix)?;
            // Extract pointee type from Ref type
            match &prefix_ty {
                Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
                    RigidName::Ref(_) => {
                        // Parameters: [Lt, pointee_ty]
                        match rigid_ty.parameters.get(1) {
                            Some(Parameter::Ty(ty)) => Ok(ty.as_ref().clone()),
                            _ => anyhow::bail!("ref type missing pointee parameter"),
                        }
                    }
                    _ => anyhow::bail!("deref on non-reference type: {prefix_ty:?}"),
                },
                _ => anyhow::bail!("deref on non-reference type: {prefix_ty:?}"),
            }
        }
        PlaceExprData::Field { prefix, field_name } => {
            let prefix_ty = infer_place_ty(state, prefix)?;
            match &prefix_ty {
                Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
                    RigidName::AdtId(adt_id) => {
                        let (_, field_ty) = struct_field_index(
                            state.builder.crates,
                            adt_id,
                            &rigid_ty.parameters,
                            field_name,
                        )?;
                        Ok(field_ty)
                    }
                    _ => anyhow::bail!("field access on non-struct type: {prefix_ty:?}"),
                },
                _ => anyhow::bail!("field access on non-struct type: {prefix_ty:?}"),
            }
        }
        _ => anyhow::bail!("cannot infer type of place expression"),
    }
}
