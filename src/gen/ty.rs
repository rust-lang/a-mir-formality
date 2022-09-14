use rustc_middle::ty;

use crate::gen::FormalityGen;

impl<'tcx> FormalityGen<'tcx> {
    pub fn emit_where_clause(&self, pred: &ty::Predicate<'tcx>, is_bounds_clause: bool) -> String {
        let clause = match pred.kind().skip_binder() {
            ty::PredicateKind::Trait(trait_pred) => {
                let trait_name = self.emit_def_path(trait_pred.def_id());

                let params = trait_pred
                    .trait_ref
                    .substs
                    .iter()
                    .skip(1) // skip the self type
                    .map(|arg| self.emit_user_param(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                if is_bounds_clause {
                    format!("({trait_name}[{params}])")
                } else {
                    let self_ty = self.emit_user_ty(trait_pred.self_ty());
                    format!("({self_ty} : {trait_name}[{params}])")
                }
            }
            ty::PredicateKind::RegionOutlives(outlives_pred) => {
                assert!(!is_bounds_clause);
                format!(
                    "((lifetime {}) : (lifetime {}))",
                    self.emit_lifetime(outlives_pred.0),
                    self.emit_lifetime(outlives_pred.1)
                )
            }
            ty::PredicateKind::TypeOutlives(outlives_pred) => {
                if is_bounds_clause {
                    self.emit_lifetime(outlives_pred.1)
                } else {
                    let self_ty = self.emit_user_ty(outlives_pred.0);
                    format!(
                        "((type {}) : (lifetime {})))",
                        self_ty,
                        self.emit_lifetime(outlives_pred.1)
                    )
                }
            }
            ty::PredicateKind::Projection(proj_pred) => {
                let rhs_ty =
                    self.emit_user_ty(proj_pred.term.ty().unwrap_or_else(|| unimplemented!()));
                format!(
                    "({} == {rhs_ty})",
                    self.emit_projection(proj_pred.projection_ty, !is_bounds_clause)
                )
            }
            _ => unimplemented!(),
        };

        if pred.kind().bound_vars().is_empty() {
            clause
        } else {
            let vars = pred
                .kind()
                .bound_vars()
                .iter()
                .map(|var| self.emit_bound_var(var))
                .intersperse(" ".to_string())
                .collect::<String>();

            format!("(for[{vars}] {clause})")
        }
    }

    pub fn emit_user_ty(&self, ty: ty::Ty<'tcx>) -> String {
        match ty.kind() {
            ty::TyKind::Bool => "bool".into(),
            ty::TyKind::Int(int_ty) => int_ty.name_str().into(),
            ty::TyKind::Uint(uint_ty) => uint_ty.name_str().into(),
            ty::TyKind::Adt(adt_def, substs) => {
                let def_path = self.emit_def_path(adt_def.did());
                let substs_str = substs
                    .iter()
                    .map(|arg| self.emit_user_param(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!("({def_path} < {substs_str} >)")
            }
            ty::TyKind::Ref(lt, ty, is_mut) => {
                let lifetime_str = self.emit_lifetime(*lt);
                let ty_str = self.emit_user_ty(*ty);
                match is_mut {
                    rustc_hir::Mutability::Not => format!("(& {lifetime_str} {ty_str})"),
                    rustc_hir::Mutability::Mut => format!("(&mut {lifetime_str} {ty_str})"),
                }
            }
            ty::TyKind::FnDef(_, _) => todo!(),
            ty::TyKind::FnPtr(fn_sig) => {
                let inputs = fn_sig
                    .skip_binder()
                    .inputs()
                    .iter()
                    .map(|ty| self.emit_user_ty(*ty))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                let output = self.emit_user_ty(fn_sig.skip_binder().output());
                let fn_ty = format!("(fn ({inputs}) -> {output})");

                if fn_sig.bound_vars().is_empty() {
                    fn_ty
                } else {
                    let vars = fn_sig
                        .bound_vars()
                        .iter()
                        .map(|var| self.emit_bound_var(var))
                        .intersperse(" ".to_string())
                        .collect::<String>();

                    format!("(for[{vars}] {fn_ty})")
                }
            }
            ty::TyKind::Tuple(substs) => {
                if substs.is_empty() {
                    "()".to_string()
                } else {
                    let substs_str = substs
                        .into_iter()
                        .map(|ty| self.emit_user_ty(ty))
                        .intersperse(" ".to_string())
                        .collect::<String>();

                    format!("(tuple {substs_str})")
                }
            }
            ty::TyKind::Projection(projection_ty) => {
                let projection_ty: &ty::ProjectionTy<'tcx> = projection_ty;
                format!("({})", self.emit_projection(*projection_ty, true),)
            }
            ty::TyKind::Param(param_ty) => format!("{}", param_ty.name),
            _ => unimplemented!(),
        }
    }

    /// Used to emit types and predicates.
    /// For bounds clauses, the self type is not included.
    fn emit_projection(&self, projection_ty: ty::ProjectionTy<'tcx>, include_self: bool) -> String {
        let assoc_item = self.tcx.associated_item(projection_ty.item_def_id);
        let trait_def_id = assoc_item.container_id(self.tcx);
        let trait_name = self.emit_def_path(trait_def_id);
        let trait_generics = self.tcx.generics_of(trait_def_id);

        // split projection_ty.substs into params of the trait and params of the assoc type
        let trait_params = projection_ty
            .substs
            .iter()
            .take(trait_generics.count())
            .skip(1) // skip self type
            .map(|arg| self.emit_user_param(arg))
            .intersperse(" ".to_string())
            .collect::<String>();

        let assoc_params = projection_ty
            .substs
            .iter()
            .skip(trait_generics.count())
            .map(|arg| self.emit_user_param(arg))
            .intersperse(" ".to_string())
            .collect::<String>();

        if include_self {
            let self_ty = self.emit_user_ty(projection_ty.self_ty());
            format!(
                "< {self_ty} as {trait_name}[{trait_params}] > :: {}[{assoc_params}]",
                assoc_item.name,
            )
        } else {
            format!(
                "{trait_name}[{trait_params}] :: {}[{assoc_params}]",
                assoc_item.name,
            )
        }
    }

    pub fn emit_ty(&self, ty: ty::Ty<'tcx>) -> String {
        if let ty::TyKind::Param(param_ty) = ty.kind() {
            format!("{}", param_ty.name)
        } else {
            // convert UserTy to Ty by calling the user-ty metafunction
            format!("(mf-apply user-ty {})", self.emit_user_ty(ty))
        }
    }

    pub fn emit_user_param(&self, generic_arg: ty::subst::GenericArg<'tcx>) -> String {
        match generic_arg.unpack() {
            ty::subst::GenericArgKind::Lifetime(lt) => self.emit_lifetime(lt),
            ty::subst::GenericArgKind::Type(ty) => self.emit_user_ty(ty),
            ty::subst::GenericArgKind::Const(_) => unimplemented!(),
        }
    }

    pub fn emit_param(&self, generic_arg: ty::subst::GenericArg<'tcx>) -> String {
        match generic_arg.unpack() {
            ty::subst::GenericArgKind::Lifetime(lt) => self.emit_lifetime(lt),
            ty::subst::GenericArgKind::Type(ty) => self.emit_ty(ty),
            ty::subst::GenericArgKind::Const(_) => unimplemented!(),
        }
    }

    pub fn emit_generic_param(&self, param: &'tcx ty::GenericParamDef) -> String {
        let kind = match param.kind {
            ty::GenericParamDefKind::Lifetime => "lifetime",
            ty::GenericParamDefKind::Type { .. } => "type",
            _ => unimplemented!(),
        };
        format!("({kind} {})", param.name)
    }

    pub fn emit_bound_var(&self, bound_var: ty::BoundVariableKind) -> String {
        match bound_var {
            // lifetime names are prefixed with %
            ty::BoundVariableKind::Region(br_kind) => match br_kind {
                ty::BoundRegionKind::BrNamed(_, name) => {
                    format!("(lifetime {})", self.emit_ident(&name))
                }
                ty::BoundRegionKind::BrAnon(idx) => {
                    format!("(lifetime {}{idx})", Self::LIFETIME_MARKER)
                }
                _ => format!("(lifetime {br_kind:?})"),
            },
            ty::BoundVariableKind::Ty(bt_kind) => format!("(type {bt_kind:?})"),
            _ => unimplemented!(),
        }
    }

    pub fn emit_lifetime(&self, lifetime: ty::Region<'tcx>) -> String {
        match lifetime.kind() {
            ty::RegionKind::ReStatic => "static".to_string(),
            ty::RegionKind::ReEarlyBound(re) => format!("{}", self.emit_ident(&re.name)),
            ty::RegionKind::ReLateBound(_, bound_region) => match bound_region.kind {
                ty::BoundRegionKind::BrNamed(_, name) => format!("{}", self.emit_ident(&name)),
                ty::BoundRegionKind::BrAnon(idx) => format!("{}{idx}", Self::LIFETIME_MARKER),
                _ => format!("{:?}", bound_region.kind),
            },
            ty::RegionKind::ReErased => "?".to_string(),
            ty::RegionKind::ReVar(vid) => format!("?{}", vid.index()),
            _ => unimplemented!(),
        }
    }
}
