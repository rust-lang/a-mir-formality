//! CodegenGlobal, CodegenFn, and CodegenScope: the state threaded through codegen judgments.

use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::FlowState;
use crate::grammar::{expr::LabelId, Crates, Fallible, Parameter, Ty, ValueId, Wcs};
use crate::prove::prove::{Env, Program};
use formality_core::Upcast;
use libspecr::prelude::Map;
use minirust_rs::lang;
use std::sync::Arc;

use super::minirust::*;
use super::seme_region::SemeRegion;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct MonoKey {
    pub(super) id: ValueId,
    pub(super) args: Vec<Parameter>,
}

impl MonoKey {
    pub fn new(id: impl Upcast<ValueId>, args: impl Upcast<Vec<Parameter>>) -> Self {
        Self {
            id: id.upcast(),
            args: args.upcast(),
        }
    }
}

/// Cross-function state: tracks the set of monomorphized functions discovered
/// during codegen and allocates globally-unique function names.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenGlobal {
    /// Input program.
    pub(super) crates: Crates,
    /// Monotonically increasing counter for generating unique function names.
    fn_counter: u32,
    /// Map from monomorphized call sites to MiniRust function names.
    /// Entries are added on first encounter; codegen loops until all are compiled.
    fn_map: Vec<(MonoKey, MiniRustFn)>,
}

/// Per-function state: locals, basic-block counters, type environment.
/// Created fresh for each function and not shared across functions.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenFn {
    /// Input program (cloned from global for convenience).
    pub(super) crates: Crates,
    /// Used to resolve types, prove bounds, etc.
    pub(super) typeck_env: TypeckEnv,
    /// Where-clauses in scope for the current function being generated.
    pub(super) assumptions: Wcs,
    /// Monotonically increasing counter for generating unique local names.
    local_counter: u32,
    /// Monotonically increasing counter for generating unique basic-block names.
    bb_counter: u32,
    /// Locals declared in the current function.
    pub(super) locals: Vec<(MiniRustLocal, MiniRustType)>,
}

/// Per-function scope that wraps and extends `FlowState`.
///
/// `FlowState` tracks borrow-checker state: scopes, liveness, loans, and variable
/// types. `CodegenScope` adds the mapping from source `ValueId`s to MiniRust locals,
/// the loop label stack (for resolving `break`/`continue` to basic-block targets),
/// and the return-value local.
///
/// The variable-to-local mapping (`vars`) is duplicated with `FlowState::scopes`
/// in the sense that both know which variables are in scope and their types, but
/// `vars` additionally maps each variable to its MiniRust local name, which
/// `FlowState` doesn't track.
///
/// `FlowState` must be kept in sync as we codegen: when a new variable is introduced
/// (`push_var`), it is registered in both `vars` and `FlowState`. When we call into
/// the borrow-checker (e.g., `resolve_place`), we pass the current `flow_state` so
/// it can verify liveness and loan validity at this program point.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenScope {
    /// Source variable name → (MiniRust local, original Rust type).
    vars: Vec<(ValueId, MiniRustLocal, Ty)>,
    /// Stack of enclosing loops, used to resolve `break`/`continue` targets.
    label_scopes: Vec<LabelScope>,
    /// The local where the function's return value is written.
    pub(super) ret_local: MiniRustLocal,
    /// Borrow-checker flow state (liveness, loans) at this program point.
    pub(super) flow_state: FlowState,
}

/// One entry in the label scope stack, representing an enclosing loop.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct LabelScope {
    label: LabelId,
    /// Block to jump to on `continue`. `None` for non-loop labels (e.g., plain blocks).
    continue_target: Option<MiniRustBb>,
    /// Block to jump to on `break`.
    break_target: MiniRustBb,
}

formality_core::cast_impl!(CodegenGlobal);
formality_core::cast_impl!(CodegenFn);
formality_core::cast_impl!(CodegenScope);
formality_core::cast_impl!(MonoKey);

impl CodegenGlobal {
    /// Create a fresh global state for codegen over the given crates.
    pub(super) fn new(crates: &Crates) -> Self {
        CodegenGlobal {
            crates: crates.clone(),
            fn_counter: 0,
            fn_map: Vec::new(),
        }
    }

    /// Allocate a unique function name.
    pub(super) fn fresh_fn(&self) -> (lang::FnName, Self) {
        let mut this = self.clone();
        let n = this.fn_counter;
        this.fn_counter += 1;
        (lang::FnName(libspecr::Name::from_internal(n)), this)
    }

    /// Return the MiniRust function name for a monomorphized call site,
    /// allocating a new entry if this is the first time we've seen this key.
    pub(super) fn ensure_fn(&self, key: MonoKey) -> (lang::FnName, Self) {
        for (k, name) in &self.fn_map {
            if *k == key {
                return (name.0, self.clone());
            }
        }
        let (name, mut g) = self.fresh_fn();
        g.fn_map.push((key, MiniRustFn(name)));
        (name, g)
    }

    /// Return the next function in `fn_map` that hasn't been compiled yet.
    pub(super) fn next_pending(
        &self,
        done: &Map<lang::FnName, lang::Function>,
    ) -> Option<(MonoKey, lang::FnName)> {
        self.fn_map
            .iter()
            .find(|(_, n)| !done.contains_key(n.0))
            .map(|(k, n)| (k.clone(), n.0))
    }
}

impl CodegenFn {
    /// Create per-function state for compiling a function with the given return type.
    pub(super) fn new(crates: &Crates, output_ty: &Ty) -> Self {
        let program = Program {
            crates: Arc::new(crates.clone()),
            max_size: Program::DEFAULT_MAX_SIZE,
        };
        let typeck_env = TypeckEnv::for_fn_body(Env::default(), &program, output_ty);
        CodegenFn {
            crates: crates.clone(),
            typeck_env,
            assumptions: Wcs::t(),
            local_counter: 0,
            bb_counter: 0,
            locals: Vec::new(),
        }
    }

    /// Allocate a unique local name (does not register it in `locals`).
    pub(super) fn fresh_local(&self) -> (lang::LocalName, Self) {
        let mut this = self.clone();
        let n = this.local_counter;
        this.local_counter += 1;
        (lang::LocalName(libspecr::Name::from_internal(n)), this)
    }

    /// Allocate a unique basic-block name.
    pub(super) fn fresh_bb(&self) -> (lang::BbName, Self) {
        let mut this = self.clone();
        let n = this.bb_counter;
        this.bb_counter += 1;
        (lang::BbName(libspecr::Name::from_internal(n)), this)
    }

    /// Allocate a local and register it with its type in the locals list.
    pub(super) fn alloc_local(&self, ty: lang::Type) -> (lang::LocalName, Self) {
        let (name, mut f) = self.fresh_local();
        f.locals.push((MiniRustLocal(name), MiniRustType(ty)));
        (name, f)
    }

    /// Translate a formality-rust type to its MiniRust representation.
    pub(super) fn minirust_ty(&self, ty: &Ty) -> Fallible<lang::Type> {
        minirust_ty(&self.crates, ty)
    }

    /// Create a new empty anonymous `SemeRegion`.
    pub(super) fn fresh_region(&self) -> SemeRegion {
        SemeRegion::new()
    }

    /// Allocate a temporary local for the given Rust type.
    pub(super) fn alloc_temp(&self, ty: &Ty) -> Fallible<(MiniRustLocal, Self)> {
        let mr_ty = self.minirust_ty(ty)?;
        let (name, f) = self.alloc_local(mr_ty);
        Ok((MiniRustLocal(name), f))
    }
}

impl CodegenScope {
    pub(super) fn new(ret_local: lang::LocalName, flow_state: FlowState) -> Self {
        CodegenScope {
            vars: Vec::new(),
            label_scopes: Vec::new(),
            ret_local: MiniRustLocal(ret_local),
            flow_state,
        }
    }
    pub(super) fn lookup_var(&self, id: &ValueId) -> Fallible<(lang::LocalName, Ty)> {
        self.vars
            .iter()
            .rev()
            .find(|(n, _, _)| n == id)
            .map(|(_, l, t)| (l.0, t.clone()))
            .ok_or_else(|| anyhow::anyhow!("unbound variable `{id:?}`"))
    }
    pub(super) fn lookup_label(
        &self,
        label: &LabelId,
    ) -> Fallible<(Option<lang::BbName>, lang::BbName)> {
        self.label_scopes
            .iter()
            .rev()
            .find(|s| s.label == *label)
            .map(|s| (s.continue_target.map(|b| b.into()), s.break_target.into()))
            .ok_or_else(|| anyhow::anyhow!("no label `{label:?}` in scope"))
    }
    pub(super) fn push_var(
        &self,
        id: impl Upcast<ValueId>,
        local: impl Upcast<MiniRustLocal>,
        ty: impl Upcast<Ty>,
    ) -> Fallible<Self> {
        let id: ValueId = id.upcast();
        let local: MiniRustLocal = local.upcast();
        let ty: Ty = ty.upcast();
        let mut s = self.clone();
        s.flow_state = s
            .flow_state
            .with_local_in_scope(&Env::default(), &None, &id, &ty)?;
        s.vars.push((id, local, ty));
        Ok(s)
    }
    /// Add a variable mapping without updating FlowState.
    /// Used for input args that are already registered in FlowState via for_fn_body.
    pub(super) fn push_var_no_flow(mut self, id: ValueId, local: lang::LocalName, ty: Ty) -> Self {
        self.vars.push((id, MiniRustLocal(local), ty));
        self
    }
    pub(super) fn with_label(
        &self,
        label: impl Upcast<LabelId>,
        continue_target: impl Upcast<Option<MiniRustBb>>,
        break_target: impl Upcast<MiniRustBb>,
    ) -> Self {
        let label: LabelId = label.upcast();
        let continue_target: Option<MiniRustBb> = continue_target.upcast();
        let break_target: MiniRustBb = break_target.upcast();
        let mut s = self.clone();
        s.label_scopes.push(LabelScope {
            label,
            continue_target: continue_target.upcast(),
            break_target,
        });
        s
    }
}
