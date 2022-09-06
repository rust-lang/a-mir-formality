use crate::gen::FormalityGen;

use mir::interpret::{ConstValue, Scalar};
use rustc_const_eval::interpret::GlobalAlloc;
use rustc_middle::{mir, ty};

impl<'tcx> FormalityGen<'tcx> {
    fn emit_scalar_int(&self, int: ty::ScalarInt, ty: ty::Ty<'tcx>) -> Option<String> {
        match ty.kind() {
            ty::Bool if int == ty::ScalarInt::FALSE => Some("false".to_string()),
            ty::Bool if int == ty::ScalarInt::TRUE => Some("true".to_string()),
            ty::Uint(_) | ty::Int(_) => Some(int.to_string()),
            _ => None,
        }
    }

    fn emit_ty_const(&self, ct: ty::Const<'tcx>) -> Option<String> {
        let ty = ct.ty();
        match ct.kind() {
            ty::ConstKind::Value(valtree) => match (valtree, ty.kind()) {
                (ty::ValTree::Branch(_), ty::Tuple(..)) => {
                    let contents = self.tcx.destructure_const(ct);
                    let fields = contents
                        .fields
                        .iter()
                        .flat_map(|field| self.emit_ty_const(*field))
                        .intersperse(" ".to_string())
                        .collect::<String>();

                    Some(format!("(tuple [{fields}])"))
                }
                (ty::ValTree::Leaf(int), _) => self.emit_scalar_int(int, ty),
                _ => None,
            },
            _ => None,
        }
    }

    fn emit_const_fn_ptr(
        &self,
        def_id: rustc_hir::def_id::DefId,
        substs: ty::SubstsRef<'tcx>,
    ) -> String {
        let fn_id = self.emit_def_path(def_id);
        let params = substs
            .iter()
            .map(|arg| self.emit_generic_arg(arg))
            .intersperse(" ".to_string())
            .collect::<String>();

        format!("(fn-ptr {fn_id} [{params}])")
    }

    fn emit_mir_const(&self, ct: ConstValue<'tcx>, ty: ty::Ty<'tcx>) -> Option<String> {
        match (ct, ty.kind()) {
            (_, ty::Tuple(..)) => {
                let contents = self.tcx.try_destructure_mir_constant(
                    ty::ParamEnv::reveal_all().and(mir::ConstantKind::Val(ct, ty)),
                )?;

                let fields = contents
                    .fields
                    .iter()
                    .flat_map(|field| match field {
                        mir::ConstantKind::Ty(ct) => self.emit_ty_const(*ct),
                        mir::ConstantKind::Val(ct, ty) => self.emit_mir_const(*ct, *ty),
                    })
                    .intersperse(" ".to_string())
                    .collect::<String>();

                Some(format!("(tuple [{fields}])"))
            }
            (ConstValue::Scalar(Scalar::Int(int)), _) => self.emit_scalar_int(int, ty),
            (ConstValue::Scalar(Scalar::Ptr(ptr, _)), ty::FnPtr(_)) => {
                let (alloc_id, _) = ptr.into_parts();
                if let GlobalAlloc::Function(instance) = self.tcx.try_get_global_alloc(alloc_id)? {
                    Some(self.emit_const_fn_ptr(instance.def_id(), instance.substs))
                } else {
                    None
                }
            }
            (ConstValue::Scalar(Scalar::Ptr(ptr, _)), _) => {
                let (alloc_id, _) = ptr.into_parts();
                if let GlobalAlloc::Static(def_id) = self.tcx.try_get_global_alloc(alloc_id)? {
                    Some(format!("(static {})", self.emit_def_path(def_id)))
                } else {
                    None
                }
            }
            (ConstValue::ZeroSized, ty::FnDef(def_id, substs)) => {
                Some(self.emit_const_fn_ptr(*def_id, *substs))
            }
            _ => None,
        }
    }

    pub fn emit_const(&self, constant: mir::Constant<'tcx>) -> String {
        match constant.literal {
            mir::ConstantKind::Ty(ct) => self.emit_ty_const(ct),
            mir::ConstantKind::Val(ct, ty) => self.emit_mir_const(ct, ty),
        }
        .unwrap_or_else(|| {
            eprintln!("unknown const: {constant:?}");
            "unknown-const".to_string()
        })
    }
}
