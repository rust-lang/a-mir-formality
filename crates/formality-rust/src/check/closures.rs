use crate::grammar::{ClosureDef, ClosureDefBoundData, CrateId, Fallible, MaybeFnBody, Wcs};
use crate::prove::prove::Env;
use crate::prove::ToWcs;
use formality_core::judgment::ProofTree;

use crate::check::Check;

impl Check<'_> {
    pub(crate) fn check_closure_def(
        &self,
        c: &ClosureDef,
        crate_id: &CrateId,
    ) -> Fallible<ProofTree> {
        let ClosureDef { id: _, binder } = c;
        let mut env = Env::default();
        let ClosureDefBoundData {
            captures,
            input_args,
            output_ty,
            where_clauses,
            body,
        } = env.instantiate_universally(binder);

        let fn_assumptions: Wcs = where_clauses.to_wcs();

        let mut proof_tree = ProofTree::leaf(format!("check_closure_def({c:?})"));

        // Check well-formedness of where-clauses
        proof_tree
            .children
            .push(self.prove_where_clauses_well_formed(&env, &fn_assumptions, &where_clauses)?);

        // Check well-formedness of capture types
        for capture in &captures {
            proof_tree.children.push(self.prove_goal(
                &env,
                &fn_assumptions,
                capture.ty.well_formed(),
            )?);
        }

        // Check well-formedness of input types
        for input_arg in &input_args {
            proof_tree.children.push(self.prove_goal(
                &env,
                &fn_assumptions,
                input_arg.ty.well_formed(),
            )?);
        }

        // Check well-formedness of output type
        proof_tree.children.push(self.prove_goal(
            &env,
            &fn_assumptions,
            output_ty.well_formed(),
        )?);

        // Type-check the body if present.
        // Captures are treated as input args to the body.
        match body {
            MaybeFnBody::NoFnBody => {}
            MaybeFnBody::FnBody(fn_body) => match fn_body {
                crate::grammar::FnBody::TrustedFnBody => {}
                crate::grammar::FnBody::Literal(_, _) => todo!(),
                crate::grammar::FnBody::MiniRust(body) => {
                    // Captures become input arguments for the body
                    let capture_args: Vec<_> = captures
                        .iter()
                        .map(|cap| crate::grammar::InputArg {
                            id: cap.id.clone(),
                            ty: cap.ty.clone(),
                        })
                        .collect();
                    let all_args: Vec<_> = capture_args
                        .into_iter()
                        .chain(input_args.into_iter())
                        .collect();
                    proof_tree.children.push(self.check_body(
                        &env,
                        &output_ty,
                        &fn_assumptions,
                        body,
                        all_args,
                        crate_id,
                    )?);
                }
            },
        }

        Ok(proof_tree)
    }
}
