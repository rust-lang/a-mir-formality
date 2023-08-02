use crate::prove;
use crate::prove::is_local::is_local_trait_ref;
use crate::prove::is_local::may_be_remote;
use crate::prove::prove_after::prove_after;
use crate::prove::prove_eq::prove_eq;
use crate::prove::prove_via::prove_via;
use crate::prove::prove_wf::prove_wf;
use crate::Decls;
use crate::Env;

use crate::Constraints;
use formality_core::{judgment_fn, ProvenSet, Upcasted};
use formality_types::grammar::{
    Const, ExhaustiveState, Parameter, Predicate, Relation, Scalar, Ty, Wc, WcData, Wcs,
};

pub fn is_covering(vals: &[ExhaustiveState], params: &[Parameter]) -> Wcs {
    assert_eq!(vals.len(), params.len());
    vals.iter()
        .zip(params.iter())
        .filter_map(|(a, b)| match a {
            ExhaustiveState::ExactMatch => None,
            ExhaustiveState::ConstCover(cs) => {
                let Parameter::Const(c) = b else {
                    todo!();
                };
                Some(Predicate::Covers(cs.clone(), c.clone()))
            }
        })
        .upcasted()
        .collect()
}

judgment_fn! {
    pub fn prove_wc(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env, decls)

        (
            (let (env, subst) = env.universal_substitution(&binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(decls, env, &assumptions, p1) => c)
            --- ("forall")
            (prove_wc(decls, env, assumptions, WcData::ForAll(binder)) => c.pop_subst(&subst))
        )

        (
            (prove_wc(decls, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(decls, env, assumptions, WcData::Implies(p1, p2)) => c)
        )

        (
            (&assumptions => a)!
            (prove_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption")
            (prove_wc(decls, env, assumptions, WcData::PR(goal)) => c)
        )

        (
            (let mut covering_consts = vec![ExhaustiveState::ExactMatch; trait_ref.parameters.len()])
            (let asmp = &assumptions)
            (let d = &decls)
            (d.impl_decls(&trait_ref.trait_id).flat_map(|i| {

              let (env, subst) = env.clone().universal_substitution(&i.binder);
              let i = i.binder.instantiate_with(&subst).unwrap();
              let co_assumptions = (asmp, &trait_ref);
              let cs = prove(
                &decls, env, &co_assumptions,
                Wcs::eq_or_cover(
                  &i.trait_ref.parameters, &trait_ref.parameters, &mut covering_consts
                )
              );
              let cs = cs.flat_map(move |c| prove_after(d, c, &co_assumptions, &i.where_clause));
              let cs = cs.flat_map(move |c| {
                  let t = d.trait_decl(&i.trait_ref.trait_id)
                    .binder.instantiate_with(&i.trait_ref.parameters).unwrap();
                  prove_after(d, c, asmp, &t.where_clause)
               });
               cs.into_iter()
            }).into_iter().collect::<ProvenSet<_>>() => c)
            (prove_after(d, c, asmp, is_covering(&covering_consts, &trait_ref.parameters)) => c)
            ----------------------------- ("exhaustive positive impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (decls.impl_decls(&trait_ref.trait_id) => i)!
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (let t = decls.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())
            (let co_assumptions = (&assumptions, &trait_ref))
            (prove(&decls, env, co_assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(&decls, c, co_assumptions, &i.where_clause) => c)
            (prove_after(&decls, c, &assumptions, &t.where_clause) => c)
            ----------------------------- ("positive impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (if env.is_in_coherence_mode())!
            (may_be_remote(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("coherence / remote impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.ambiguous())
        )

        (
            (decls.neg_impl_decls(&trait_ref.trait_id) => i)
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (prove(&decls, env, &assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(&decls, c, &assumptions, &i.where_clause) => c)
            ----------------------------- ("negative impl")
            (prove_wc(decls, env, assumptions, Predicate::NotImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, alias_ty, ty) => c)
            ----------------------------- ("alias eq")
            (prove_wc(decls, env, assumptions, Predicate::AliasEq(alias_ty, ty)) => c)
        )

        (
            (decls.trait_invariants() => ti)
            (let (env, subst) = env.existential_substitution(&ti.binder))
            (let ti = ti.binder.instantiate_with(&subst).unwrap())
            (prove_via(&decls, env, &assumptions, &ti.where_clause, &trait_ref) => c)
            (prove_after(&decls, c, &assumptions, &ti.trait_ref) => c)
            ----------------------------- ("trait implied bound")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_wc(decls, env, assumptions, Relation::Equals(a, b)) => c)
        )

        (
            (let t = decls.trait_decl(&trait_ref.trait_id))
            (let t = t.binder.instantiate_with(&trait_ref.parameters).unwrap())
            (prove(decls, env, assumptions, t.where_clause) => c)
            ----------------------------- ("trait well formed")
            (prove_wc(decls, env, assumptions, Predicate::WellFormedTraitRef(trait_ref)) => c)
        )

        (
            (is_local_trait_ref(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("trait ref is local")
            (prove_wc(decls, env, assumptions, Predicate::IsLocal(trait_ref)) => c)
        )

        (
            (let () = vals.sort_unstable())
            (prove(&decls, env, &assumptions, Predicate::ConstHasType(var, Ty::bool())) => c)
            (vals.iter().cloned() => v)
            (prove_after(&decls, &c, &assumptions, Predicate::ConstHasType(v, Ty::bool())) => c)
            (if vals.len() == 2)
            (vals.clone().into_iter().enumerate().flat_map(|(i, v)| {
                prove_after(
                  &decls, &c, &assumptions,
                  Relation::Equals(Parameter::Const(Const::valtree(Scalar::new(i as u128),
                  Ty::bool())), Parameter::Const(v))
                ).into_iter()
            }).collect::<ProvenSet<_>>() => c)
            ----------------------------- ("exhaustive bool values cover variable")
            (prove_wc(decls, env, assumptions, Predicate::Covers(mut vals, var)) => c)
        )


        (
            (prove_wf(decls, env, assumptions, p) => c)
            ----------------------------- ("parameter well formed")
            (prove_wc(decls, env, assumptions, Relation::WellFormed(p)) => c)
        )

        (
            (if let Some((_, const_ty)) = ct.as_value())
            (prove(decls, env, assumptions, Wcs::all_eq(vec![const_ty], vec![ty])) => c)
            ----------------------------- ("const has ty")
            (prove_wc(decls, env, assumptions, Predicate::ConstHasType(ct, ty)) => c)
        )
    }
}
