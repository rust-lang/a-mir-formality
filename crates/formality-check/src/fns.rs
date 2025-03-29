use formality_prove::Env;
use formality_rust::{
    grammar::{Fn, FnBody, FnBoundData, MaybeFnBody},
    prove::ToWcs,
};
use formality_types::grammar::{Fallible, Relation, Wcs};

use crate::Check;

impl Check<'_> {
    pub(crate) fn check_free_fn(&self, f: &Fn) -> Fallible<()> {
        self.check_fn(&Env::default(), Wcs::t(), f)
    }

    pub(crate) fn check_fn(
        &self,
        in_env: &Env,
        in_assumptions: impl ToWcs,
        f: &Fn,
    ) -> Fallible<()> {
        let in_assumptions = in_assumptions.to_wcs();
        assert!(in_env.only_universal_variables() && in_env.encloses((&in_assumptions, f)));

        let mut env = in_env.clone();

        let Fn { id: _, binder } = f;

        let FnBoundData {
            input_tys,
            output_ty,
            where_clauses,
            effect: declared_effect,
            body,
        } = env.instantiate_universally(binder);

        let fn_assumptions: Wcs = (in_assumptions, &where_clauses).to_wcs();

        self.prove_where_clauses_well_formed(&env, &fn_assumptions, &where_clauses)?;

        for input_ty in &input_tys {
            self.prove_goal(&env, &fn_assumptions, input_ty.well_formed())?;
        }

        self.prove_goal(&env, &fn_assumptions, output_ty.well_formed())?;

        // prove that the body type checks (if present)
        eprintln!("check_fn: {body:?}");
        match body {
            MaybeFnBody::NoFnBody => {
                // No fn body: this occurs e.g. for the fn `bar`
                // in `trait Foo { fn bar(); }`. No check required.
            }
            MaybeFnBody::FnBody(fn_body) => match fn_body {
                FnBody::TrustedFnBody => {
                    // Trusted: just assume it's ok, useful for tests
                    // to indicate things we don't want to think about.
                }
                FnBody::MirFnBody(_mir_fn_body) => {
                    // TODO: this nikomatsakis is theoretically working on removing
                    // though he has not made much time for it.
                    unimplemented!()
                }
                FnBody::FnBodyWithEffect(body_effect) => {
                    // Useful for tests: declares "some body" with the
                    // given effect. So we need to check that the effect
                    // is in bounds of the declaration.
                    self.prove_goal(
                        &env,
                        &fn_assumptions,
                        Relation::EffectSubset(body_effect, declared_effect)
                    )?;
                }
            }
        }

        Ok(())
    }
}
