use fn_error_context::context;
use formality_decl::grammar::{
    AssociatedTy, AssociatedTyBoundData, Fn, Trait, TraitBoundData, TraitItem,
};
use formality_logic::Env;
use formality_types::{
    cast::{To, Upcast},
    grammar::{Fallible, Hypothesis},
};

impl super::Check<'_> {
    #[context("check_trait({:?})", t.id)]
    pub(super) fn check_trait(&self, t: &Trait) -> Fallible<()> {
        let Trait { id, binder } = t;
        let mut env = Env::default();

        assert_eq!(t, self.trait_named(id)?);

        let TraitBoundData {
            where_clauses,
            trait_items,
        } = env.instantiate_universally(binder);

        let assumptions: Vec<Hypothesis> = where_clauses.to();

        self.check_trait_items_have_unique_names(&trait_items)?;

        self.prove_where_clauses_well_formed(&env, &assumptions, &where_clauses)?;

        for trait_item in &trait_items {
            self.check_trait_item(&env, &assumptions, trait_item)?;
        }

        Ok(())
    }

    fn check_trait_items_have_unique_names(&self, _trait_items: &[TraitItem]) -> Fallible<()> {
        // FIXME:
        Ok(())
    }

    fn check_trait_item(
        &self,
        env: &Env,
        assumptions: &[Hypothesis],
        trait_item: &TraitItem,
    ) -> Fallible<()> {
        match trait_item {
            TraitItem::Fn(v) => self.check_fn_in_trait(env, assumptions, v),
            TraitItem::AssociatedTy(v) => self.check_associated_ty(env, assumptions, v),
        }
    }

    fn check_fn_in_trait(&self, env: &Env, assumptions: &[Hypothesis], f: &Fn) -> Fallible<()> {
        self.check_fn(env, assumptions, f)
    }

    fn check_associated_ty(
        &self,
        trait_env: &Env,
        trait_assumptions: &[Hypothesis],
        associated_ty: &AssociatedTy,
    ) -> Fallible<()> {
        let mut env = trait_env.clone();

        let AssociatedTy { id: _, binder } = associated_ty;
        let AssociatedTyBoundData {
            ensures: _,
            where_clauses,
        } = env.instantiate_universally(binder);

        let assumptions: Vec<Hypothesis> = (trait_assumptions, &where_clauses).upcast();

        self.prove_where_clauses_well_formed(&env, &assumptions, &where_clauses)?;

        // FIXME: Do we prove ensures WF? And what do we assume when we do so?

        Ok(())
    }
}
