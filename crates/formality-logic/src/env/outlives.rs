use formality_types::{
    cast::{To, Upcast, Upcasted},
    db::Db,
    derive_links::Parameter,
    grammar::{
        Binder, ElaboratedHypotheses, EnsuresTy, Fallible, Goal, ImplicationTy, LtData,
        ParameterData, PlaceholderVar, PredicateTy, RigidName, RigidTy, Ty, TyData, Variable,
    },
    seq,
};

use super::{
    bound::{PlaceholderBound, PlaceholderBoundData},
    extrude::Relationship,
    Env,
};

impl Env {
    /// Require `a : b`, yielding a new environment + list of goals that must all be solved for `a : b` to be true.
    /// Returns `Err` if the two parameters can never be related.
    pub(super) fn outlives(
        &self,
        db: &Db,
        assumptions: &ElaboratedHypotheses,
        a: &Parameter,
        b: &Parameter,
    ) -> Fallible<(Env, Vec<Goal>)> {
        let mut env = self.clone();
        let a = env.refresh_inference_variables(a);
        let b = env.refresh_inference_variables(b);
        let goals = env.outlives_parameters(db, assumptions, &a, &b)?;
        Ok((env, goals))
    }

    fn outlives_parameters(
        &mut self,
        _db: &Db,
        assumptions: &ElaboratedHypotheses,
        a: &Parameter,
        b: &Parameter,
    ) -> Fallible<Vec<Goal>> {
        match (a.data(), b.data()) {
            (ParameterData::Lt(LtData::Static), _) => Ok(vec![]),

            // (&'a T : P) or (&'a mut T : P) if
            //     'a : P
            //
            // This is a hack / optimization on the more general case below that is based on knowing
            // that `&'a T` is only well-formed it `T : 'a`. The general rule would have us prove
            // that *both* `T : P` and `'a : P`. But if `T : 'a` and `'a : P`, then `T : P` is implied by
            // transitivity, so we can check check whether `'a : P` and that should be good enough.
            //
            // We could do this for arbitrary rigid types if we inspected their where-clauses,
            // but I didn't feel like writing up that logic just now, and the rule below is not wrong,
            // it just produces more answers than are necessary. (So, alternatively, we could also
            // work on the "answer subsumption" logic.)
            (
                ParameterData::Ty(TyData::RigidTy(RigidTy {
                    name: RigidName::Ref(_),
                    parameters,
                })),
                _,
            ) => {
                let lt = &parameters[0];
                Ok(seq![Goal::outlives(lt, b)])
            }

            // R<Pr_0...Pr_n> : Pb if ∀i (Pr_i : Pb)
            (ParameterData::Ty(TyData::RigidTy(r)), _) => {
                Ok(r.parameters.iter().map(|p| Goal::outlives(p, b)).collect())
            }

            // Pa : R<Pr_0...Pr_n> if ∃i (Pa : Pr_i)
            // Pa : R if Pa : static
            (_, ParameterData::Ty(TyData::RigidTy(r))) => {
                if r.parameters.len() >= 1 {
                    Ok(seq![Goal::any(
                        r.parameters
                            .iter()
                            .map(|p| Goal::outlives(a, p))
                            .collect::<Vec<_>>(),
                    )])
                } else {
                    Ok(seq![Goal::outlives(a, LtData::Static)])
                }
            }

            // A<Pa ...> : P if
            //      ∀i. (Pa_i : P) or
            //      (A<Pa ...> ~~~> T; T : P)
            //
            // To establish that an alias type outlives P we can either normalize the alias type or
            // we can relate P to each of the alias type's parameters. The latter is based on the reasoning
            // that the alias must be a function of its inputs and other static things.
            (ParameterData::Ty(TyData::AliasTy(a)), _) => {
                let parameters_goal = Goal::all(
                    a.parameters
                        .iter()
                        .map(|p| Goal::outlives(p, b))
                        .collect::<Vec<_>>(),
                );

                let alias_goal = Goal::exists_f(|t: Ty| {
                    Goal::all(vec![a.normalizes_to(&t).upcast(), Goal::outlives(&t, b)])
                });

                Ok(vec![Goal::any(vec![alias_goal, parameters_goal])])
            }

            // P : A<Pa ...> if
            //      (A<Pa ...> ~~~> T; P : T) or
            //      P : static
            //
            // To establish that P outlives an alias type we *must* normalize.
            // No matter what its arguments, the alias type could normalize to `i32` or some such thing,
            // in which case only static outlives it.
            (_, ParameterData::Ty(TyData::AliasTy(b))) => {
                let alias_goal = Goal::exists_f(|t: Ty| {
                    Goal::all(seq![b.normalizes_to(&t).upcast(), Goal::outlives(a, &t)])
                });

                Ok(seq![Goal::any(seq![
                    alias_goal,
                    Goal::outlives(a, LtData::Static),
                ])])
            }

            (ParameterData::Ty(TyData::PredicateTy(PredicateTy::Exists(a))), _) => {
                let (names, ty) = a.open();
                Ok(vec![Goal::for_all(&names, Goal::outlives(ty, b))])
            }

            (_, ParameterData::Ty(TyData::PredicateTy(PredicateTy::ForAll(b)))) => {
                let (names, ty) = b.open();
                Ok(vec![Goal::for_all(&names, Goal::outlives(a, ty))])
            }

            (ParameterData::Ty(TyData::PredicateTy(PredicateTy::ForAll(a))), _) => {
                let (names, ty) = a.open();
                Ok(vec![Goal::exists(&names, Goal::outlives(ty, b))])
            }

            (_, ParameterData::Ty(TyData::PredicateTy(PredicateTy::Exists(b)))) => {
                let (names, ty) = b.open();
                Ok(vec![Goal::exists(&names, Goal::outlives(a, ty))])
            }

            (
                _,
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::EnsuresTy(EnsuresTy {
                    ty,
                    predicates,
                }))),
            ) => Ok(seq![Goal::implies(predicates, Goal::outlives(a, ty))]),

            (
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::ImplicationTy(ImplicationTy {
                    predicates,
                    ty,
                }))),
                _,
            ) => Ok(seq![Goal::implies(predicates, Goal::outlives(ty, b))]),

            (
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::EnsuresTy(EnsuresTy {
                    ty,
                    predicates,
                }))),
                _,
            ) => Ok(seq![Goal::outlives(ty, b), ..predicates.upcasted()]),

            (
                _,
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::ImplicationTy(ImplicationTy {
                    predicates,
                    ty,
                }))),
            ) => Ok(seq![Goal::outlives(a, ty), ..predicates.upcasted()]),

            (
                ParameterData::Lt(LtData::Variable(a_var)),
                ParameterData::Lt(LtData::Variable(b_var)),
            )
            | (
                ParameterData::Ty(TyData::Variable(a_var)),
                ParameterData::Ty(TyData::Variable(b_var)),
            )
            | (
                ParameterData::Lt(LtData::Variable(a_var)),
                ParameterData::Ty(TyData::Variable(b_var)),
            )
            | (
                ParameterData::Ty(TyData::Variable(a_var)),
                ParameterData::Lt(LtData::Variable(b_var)),
            ) => Ok(self.variable_outlives(assumptions, *a_var, *b_var)),

            (ParameterData::Lt(LtData::Variable(a_var)), ParameterData::Lt(LtData::Static))
            | (ParameterData::Ty(TyData::Variable(a_var)), ParameterData::Lt(LtData::Static)) => {
                Ok(self.variable_outlives_static(assumptions, *a_var))
            }
        }
    }

    fn variable_outlives(
        &mut self,
        assumptions: &ElaboratedHypotheses,
        a: Variable,
        b: Variable,
    ) -> Vec<Goal> {
        match (a, b) {
            (Variable::BoundVar(_), _) | (_, Variable::BoundVar(_)) => {
                panic!("unexpected bound variable")
            }

            (Variable::InferenceVar(a), Variable::InferenceVar(b)) => {
                if self.data(a).universe >= self.data(b).universe {
                    self.relate_parameter(a, Relationship::Outlives, b)
                } else {
                    self.relate_parameter(b, Relationship::OutlivedBy, a)
                }
            }

            (Variable::PlaceholderVar(a), Variable::InferenceVar(b)) => {
                if self.data(b).universe >= a.universe {
                    self.relate_parameter(b, Relationship::OutlivedBy, a)
                } else {
                    seq![self.placeholder_outlives(assumptions, a, &b)]
                }
            }

            (Variable::InferenceVar(a), Variable::PlaceholderVar(b)) => {
                if self.data(a).universe >= b.universe {
                    self.relate_parameter(a, Relationship::Outlives, b)
                } else {
                    seq![self.placeholder_outlived_by(assumptions, &a, b)]
                }
            }

            (Variable::PlaceholderVar(a), Variable::PlaceholderVar(b)) => {
                // Careful: It doesn't seem like both directions should be necessary, but you have to
                // consider conditional goals or inference variables. It could happen that you have
                // (for example) `!U2 : !U1` and `?U2 : !U1` in the environment. In that case, we have
                // to search for "outlived by" bounds on `!U1` to find `?U2` (which can be equated with `!U2`).
                // But we could also have `!U2 : ?U1` in the environment, so we have to serach the reverse.
                // If you don't have variables in the environment, though, this is strictly extra work.
                // e.g. If we have `!U2 : !U1`, or even `!U2 : X` and `X : !U1`, it doesn't matter which way
                // you search, you'll find the path eventually.
                let fwd = self.placeholder_outlives(assumptions, a, &b);
                let rev = self.placeholder_outlived_by(assumptions, &a, b);
                seq![Goal::any(seq![fwd, rev])]
            }
        }
    }

    fn variable_outlives_static(
        &mut self,
        assumptions: &ElaboratedHypotheses,
        a: Variable,
    ) -> Vec<Goal> {
        match a {
            Variable::PlaceholderVar(a) => {
                seq![self.placeholder_outlives(assumptions, a, LtData::Static)]
            }
            Variable::InferenceVar(a) => {
                self.relate_parameter(a, Relationship::Outlives, LtData::Static)
            }
            Variable::BoundVar(_) => {
                panic!("unexpected bound variable")
            }
        }
    }

    // We can prove `P_a : b` if we know `P_a : x` and we can prove `x : b`.
    fn placeholder_outlives(
        &self,
        assumptions: &ElaboratedHypotheses,
        a: PlaceholderVar,
        b: impl Upcast<Parameter>,
    ) -> Goal {
        let bounds = self.find_placeholder_bounds(assumptions, a, Relationship::Outlives);

        Goal::any(
            bounds
                .iter()
                .map(|bound| {
                    let (
                        names,
                        PlaceholderBoundData {
                            conditions,
                            placeholder,
                            relationship: _,
                            bound,
                        },
                    ) = bound.for_all.open();

                    Goal::exists(
                        &names,
                        Goal::all(seq![
                            Goal::eq(a, placeholder),
                            Goal::outlives(bound, &b),
                            ..conditions,
                        ]),
                    )
                })
                .collect::<Vec<_>>(),
        )
    }

    // We can prove `a : P_b` if we know `x : P_b` and we can prove `a : x`.
    fn placeholder_outlived_by(
        &self,
        assumptions: &ElaboratedHypotheses,
        a: impl Upcast<Parameter>,
        b: PlaceholderVar,
    ) -> Goal {
        let a: Parameter = a.upcast();

        let mut bounds = self.find_placeholder_bounds(assumptions, b, Relationship::OutlivedBy);

        // We always know that `static : P_b`, so add that as a bound,
        // but -- as a microopt -- only do that if there's no other
        // unconditional bound (which would be strictly better).
        if !bounds
            .iter()
            .any(|b| b.for_all.peek().conditions.is_empty())
        {
            bounds.push(PlaceholderBound {
                for_all: Binder::new(
                    &[],
                    PlaceholderBoundData {
                        conditions: vec![],
                        placeholder: b.to(),
                        relationship: Relationship::OutlivedBy,
                        bound: LtData::Static.upcast(),
                    },
                ),
            });
        }

        Goal::any(
            bounds
                .iter()
                .map(|bound| {
                    let (
                        names,
                        PlaceholderBoundData {
                            conditions,
                            placeholder,
                            relationship: _,
                            bound,
                        },
                    ) = bound.for_all.open();

                    Goal::exists(
                        &names,
                        Goal::all(seq![
                            Goal::eq(b, placeholder),
                            Goal::outlives(&a, bound),
                            ..conditions,
                        ]),
                    )
                })
                .collect::<Vec<_>>(),
        )
    }
}
