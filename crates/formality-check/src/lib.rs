use anyhow::bail;
use contracts::requires;
use formality_decl::grammar::{Crate, CrateItem, Program, Trait};
use formality_logic::Env;
use formality_logic::{prove_universal_goal, UniversalGoalResult};
use formality_types::{
    cast::Upcast,
    db::Db,
    grammar::{Fallible, Goal, Hypothesis, TraitId},
    term::Term,
};

pub fn check_program(program: &Program) -> Fallible<()> {
    let db = Db::new(program.clone());
    Check { program, db }.check()
}

mod adts;
mod fns;
mod impls;
mod traits;
mod where_clauses;

struct Check<'p> {
    program: &'p Program,
    db: Db,
}

impl Check<'_> {
    fn check(&self) -> Fallible<()> {
        let Program { crates } = &self.program;
        if let Some(current_crate) = crates.last() {
            self.check_current_crate(current_crate)?;
        }
        Ok(())
    }

    fn check_current_crate(&self, c: &Crate) -> Fallible<()> {
        let Crate { id: _, items } = c;
        for item in items {
            self.check_crate_item(item)?;
        }
        Ok(())
    }

    fn check_crate_item(&self, c: &CrateItem) -> Fallible<()> {
        match c {
            CrateItem::Adt(v) => self.check_adt(v),
            CrateItem::Trait(v) => self.check_trait(v),
            CrateItem::TraitImpl(v) => self.check_trait_impl(v),
        }
    }

    #[requires(goal.references_only_placeholder_variables())]
    fn prove_goal(
        &self,
        env: &Env,
        assumptions: impl Upcast<Vec<Hypothesis>>,
        goal: impl Term + Upcast<Goal>,
    ) -> Fallible<()> {
        let goal: Goal = goal.upcast();
        let assumptions = assumptions.upcast();
        match prove_universal_goal(&self.db, env, &assumptions, &goal) {
            UniversalGoalResult::Yes => Ok(()),
            UniversalGoalResult::No => bail!("could not prove `{goal:?}`"),
            UniversalGoalResult::Maybe => bail!("could not prove `{goal:?}` (ambiguous)"),
        }
    }

    fn trait_named(&self, id: &TraitId) -> Fallible<&Trait> {
        let traits: Vec<&Trait> = self
            .program
            .crates
            .iter()
            .flat_map(|c| &c.items)
            .filter_map(|ci| match ci {
                CrateItem::Adt(_) => None,
                CrateItem::Trait(v) => Some(v),
                CrateItem::TraitImpl(_) => None,
            })
            .filter(|t| t.id == *id)
            .collect();

        if traits.len() == 1 {
            Ok(traits[0])
        } else if traits.len() > 1 {
            bail!("multiple traits named `{id:?}`");
        } else {
            bail!("no trait named `{id:?}`")
        }
    }
}
