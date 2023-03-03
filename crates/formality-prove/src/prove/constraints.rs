use formality_types::{
    cast::Upcast,
    cast_impl,
    derive_links::UpcastFrom,
    fold::Fold,
    grammar::{Binder, InferenceVar, Parameter, Substitution, Variable},
    visit::Visit,
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Constraints {
    known_true: bool,
    substitution: Substitution,
}

cast_impl!(Constraints);

impl<A, B> UpcastFrom<(A, B)> for Constraints
where
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn upcast_from(term: (A, B)) -> Self {
        Constraints {
            substitution: term.upcast(),
            known_true: true,
        }
    }
}

impl Default for Constraints {
    fn default() -> Self {
        Self {
            known_true: true,
            substitution: Default::default(),
        }
    }
}

impl Constraints {
    pub fn substitution(&self) -> &Substitution {
        &self.substitution
    }

    pub fn ambiguous(self) -> Constraints {
        Self {
            known_true: false,
            ..self
        }
    }
}

pub fn merge_constraints(
    existentials: impl Upcast<Vec<Variable>>,
    c0: impl Upcast<Constraints>,
    c1: Binder<Constraints>,
) -> Binder<Constraints> {
    let c0: Constraints = c0.upcast();
    c0.assert_valid();

    let (c1_bound_vars, c1) = c1.open();
    c1.assert_valid();

    assert!(c0
        .substitution
        .domain()
        .is_disjoint(&c1.substitution.domain()));
    assert!(!c0
        .substitution
        .domain()
        .iter()
        .any(|v| occurs_in(v, &c1.substitution)));

    let existentials: Vec<Variable> = existentials.upcast();

    let c0 = c1.substitution.apply(&c0);

    // Drop any bindings `X := P` from the subst that appear in the `variables` set;
    // those are existentials that we temporarily introduced and no longer need.
    let substitution = c0
        .substitution
        .into_iter()
        .chain(c1.substitution)
        .filter(|(v, _)| !existentials.contains(&v.upcast()))
        .collect();

    let known_true = c0.known_true && c1.known_true;
    Binder::mentioned(
        (c1_bound_vars, existentials),
        Constraints {
            known_true,
            substitution,
        },
    )
}

impl Fold for Constraints {
    fn substitute(&self, substitution_fn: formality_types::fold::SubstitutionFn<'_>) -> Self {
        let c = Constraints {
            known_true: self.known_true,
            substitution: self.substitution.substitute(substitution_fn),
        };

        // not all substitutions preserve the constraint set invariant
        c.assert_valid();

        c
    }
}

impl Visit for Constraints {
    fn free_variables(&self) -> Vec<Variable> {
        let Constraints {
            known_true: _,
            substitution,
        } = self;
        substitution.free_variables()
    }

    fn size(&self) -> usize {
        let Constraints {
            known_true: _,
            substitution,
        } = self;
        substitution.size()
    }

    fn assert_valid(&self) {
        let domain = self.substitution.domain();
        let range = self.substitution.range();
        range
            .iter()
            .for_each(|t| assert!(domain.iter().all(|&v| !occurs_in(v, t))));
    }
}

pub fn occurs_in(v: impl Upcast<Variable>, t: &impl Visit) -> bool {
    let v: Variable = v.upcast();
    t.free_variables().contains(&v)
}

pub fn constrain(v: impl Upcast<InferenceVar>, p: impl Upcast<Parameter>) -> Binder<Constraints> {
    let v: InferenceVar = v.upcast();
    let p: Parameter = p.upcast();
    Binder::dummy((v, p).upcast())
}

pub fn no_constraints() -> Binder<Constraints> {
    Binder::dummy(().upcast())
}
