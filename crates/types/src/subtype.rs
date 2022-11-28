pub fn compare(env: Env, a: Parameter, b: Parameter) -> Result<(Env, Goals)> {
    match (a, b) {
        (Parameter::Ty(a), Parameter::Ty(b)) => compare_tys(env, a, b),
        (Parameter::Lt(a), Parameter::Lt(b)) => compare_lts(env, a, b),
        _ => {
            panic!("ill-kinded")
        }
    }
}

pub fn compare_tys(env: Env, a: Ty, b: Ty) -> Result<(Env, Goals)> {
    if a == b {
        Ok((Env, seq![]))
    }

    match (a.data(), b.data()) {
        (TyData::RigidTy(rigid_ty), TyData::Var(var_id))
            if env.contains_existential_var(var_id) => {}

        (TyData::Var(var_id), TyData::RigidTy(rigid_ty)) => {}
    }
}
