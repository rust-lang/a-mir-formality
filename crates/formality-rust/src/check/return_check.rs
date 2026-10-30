use crate::grammar::Fallible;
use crate::grammar::{
    expr::{Block, LabelId, Stmt},
    Ty,
};
use anyhow::bail;
use formality_core::judgment::ProofTree;
use formality_core::{judgment_fn, term, Set};

#[term]
struct ControlFlow {
    can_fall_through: bool,
    breaks: Set<LabelId>,
    continues: Set<LabelId>,
}

impl ControlFlow {
    fn fallthrough() -> Self {
        Self {
            can_fall_through: true,
            breaks: Set::new(),
            continues: Set::new(),
        }
    }

    fn returns() -> Self {
        Self {
            can_fall_through: false,
            breaks: Set::new(),
            continues: Set::new(),
        }
    }

    fn break_to(label: LabelId) -> Self {
        let mut breaks = Set::new();
        breaks.insert(label);

        Self {
            can_fall_through: false,
            breaks,
            continues: Set::new(),
        }
    }

    fn continue_to(label: LabelId) -> Self {
        let mut continues = Set::new();
        continues.insert(label);

        Self {
            can_fall_through: false,
            breaks: Set::new(),
            continues,
        }
    }

    fn then(&self, next: &Self) -> Self {
        if !self.can_fall_through {
            return self.clone();
        }

        Self {
            can_fall_through: next.can_fall_through,
            breaks: self.breaks.union(&next.breaks).cloned().collect(),
            continues: self.continues.union(&next.continues).cloned().collect(),
        }
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            can_fall_through: self.can_fall_through || other.can_fall_through,
            breaks: self.breaks.union(&other.breaks).cloned().collect(),
            continues: self.continues.union(&other.continues).cloned().collect(),
        }
    }

    fn exit_block(&self, label: Option<&LabelId>) -> Self {
        let mut breaks = self.breaks.clone();
        let matching_break = label.is_some_and(|label| breaks.remove(label));

        Self {
            can_fall_through: self.can_fall_through || matching_break,
            breaks,
            continues: self.continues.clone(),
        }
    }

    fn exit_loop(&self, label: Option<&LabelId>) -> Self {
        let mut breaks = self.breaks.clone();
        let mut continues = self.continues.clone();

        let matching_break = if let Some(label) = label {
            let matching_breaks = breaks.remove(label);
            continues.remove(label);

            matching_breaks
        } else {
            false
        };

        Self {
            can_fall_through: matching_break,
            breaks,
            continues,
        }
    }
}

fn check_no_fallthrough(flow: &ControlFlow) -> Fallible<ProofTree> {
    if flow.can_fall_through {
        bail!("function may not return a value");
    }

    Ok(ProofTree::leaf("function does not fall through"))
}

judgment_fn! {
  /// Checks whether every required path through a function returns a value.
  pub(crate) fn check_fn_returns( output_ty: Ty, block: Block) => () {
    debug(output_ty, block)

    (
      (if output_ty == &Ty::unit())
      ---------------------------------------------------- ("unit")
      (check_fn_returns(output_ty, block) => ())
    )

    (
      (if output_ty != &Ty::unit())
      (control_flow_block(block) => flow)
      (check_no_fallthrough(flow) => ())
      ---------------------------------------------------- ("non-unit return")
      (check_fn_returns(output_ty, block) => ())
    )
  }
}

judgment_fn! {
  /// Computes the control flow that can emerge from a block.
  fn control_flow_block( block: Block) => ControlFlow {
    debug(block)

    (
      (let flow = ControlFlow::fallthrough())
        (for_all(i in 0..stmts.len()) with(flow)
          (control_flow_stmt(&stmts[i]) => statement_flow)
            (let flow = flow.then(&statement_flow)))
      (let flow = flow.exit_block(label.as_ref().map(|label| &label.id)))
      ----------------------------------------------------- ("block")
      (control_flow_block(Block { label, stmts }) => flow)
    )
  }
}

judgment_fn! {
  /// Computes the control flow that can emerge from a statement.
  fn control_flow_stmt( stmt: Stmt) => ControlFlow {
    debug(stmt)

    (
      (let flow = ControlFlow::returns())
      ----------------------------------------------------- ("return")
      (control_flow_stmt(Stmt::Return { expr: _ }) => flow)
    )

    (
      (let flow = ControlFlow::break_to(label.clone()))
      ----------------------------------------------------- ("break")
      (control_flow_stmt(Stmt::Break { label }) => flow)
    )

    (
      (let flow = ControlFlow::continue_to(label.clone()))
      ----------------------------------------------------- ("continue")
      (control_flow_stmt(Stmt::Continue { label }) => flow)
    )

    (
      (let flow = ControlFlow::fallthrough())
      ----------------------------------------------------- ("expression")
      (control_flow_stmt(Stmt::Expr { expr: _ }) => flow)
    )

    (
      (let flow = ControlFlow::fallthrough())
      ----------------------------------------------------- ("print")
      (control_flow_stmt(Stmt::Print { expr: _ }) => flow)
    )

    (
      (let flow = ControlFlow::fallthrough())
      ----------------------------------------------------- ("let")
      (control_flow_stmt(Stmt::Let { label: _, id: _, ty: _, init: _ }) => flow)
    )

    (
      (control_flow_block(block) => flow)
      ----------------------------------------------------- ("block")
      (control_flow_stmt(Stmt::Block(block)) => flow)
    )

    (
      (control_flow_block(then_block) => then_flow)
      (control_flow_block(&else_block.block) => else_flow)
      (let flow = then_flow.join(else_flow))
      ----------------------------------------------------- ("if")
      (control_flow_stmt(Stmt::If { condition: _, then_block, else_block }) => flow)
    )

    (
      (control_flow_block(body) => body_flow)
      (let flow = body_flow.exit_loop(label.as_ref().map(|label| &label.id)))
      ----------------------------------------------------- ("loop")
      (control_flow_stmt(Stmt::Loop { label, body}) => flow)
    )

    (
      (let block = binder.peek())
      (control_flow_block(block) => flow)
      ----------------------------------------------------- ("exists")
      (control_flow_stmt(Stmt::Exists { binder }) => flow)
    )
  }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::expr::Expr;

    #[test]
    fn then_ignores_unreachable_breaks() {
        let returned = ControlFlow::returns();
        let unreachable_break = ControlFlow::break_to(LabelId::new("'break"));

        let result = returned.then(&unreachable_break);

        assert!(!result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.is_empty());
    }

    #[test]
    fn then_preserves_reachable_breaks() {
        let label = LabelId::new("'rb");
        let fallthrough = ControlFlow::fallthrough();
        let break_flow = ControlFlow::break_to(label.clone());

        let result = fallthrough.then(&break_flow);

        assert!(!result.can_fall_through);
        assert!(result.breaks.contains(&label));
        assert!(result.continues.is_empty());
    }

    #[test]
    fn then_ignores_unreachable_continues() {
        let returned = ControlFlow::returns();
        let unreachable_continue = ControlFlow::continue_to(LabelId::new("'cont"));

        let result = returned.then(&unreachable_continue);

        assert!(!result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.is_empty());
    }

    #[test]
    fn join_unions_break_and_continue_targets_from_both_branches() {
        let break_label = LabelId::new("'bl");
        let break_flow = ControlFlow::break_to(break_label.clone());
        let continue_label = LabelId::new("'cl");
        let continue_flow = ControlFlow::continue_to(continue_label.clone());

        let result = break_flow.join(&continue_flow);

        assert!(!result.can_fall_through);
        assert!(result.breaks.contains(&break_label));
        assert!(result.continues.contains(&continue_label));
    }

    #[test]
    fn join_allows_fallthrough_when_either_branch_falls_through() {
        let label = LabelId::new("'jb");
        let break_flow = ControlFlow::break_to(label.clone());
        let fallthrough = ControlFlow::fallthrough();

        let result = fallthrough.join(&break_flow);

        assert!(result.can_fall_through);
        assert!(result.breaks.contains(&label));
        assert!(result.continues.is_empty());
    }

    #[test]
    fn matching_break_exits_block() {
        let break_label = LabelId::new("'bl");
        let break_flow = ControlFlow::break_to(break_label.clone());

        let result = break_flow.exit_block(Some(&break_label));

        assert!(result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.is_empty());
    }

    #[test]
    fn continues_propagate_through_block() {
        let block_label = LabelId::new("'inner");
        let continue_label = LabelId::new("'outer");
        let continue_flow = ControlFlow::continue_to(continue_label.clone());

        let result = continue_flow.exit_block(Some(&block_label));

        assert!(!result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.contains(&continue_label));
    }

    #[test]
    fn matching_break_does_exit_loop() {
        let break_label = LabelId::new("'bl");
        let break_flow = ControlFlow::break_to(break_label.clone());

        let result = break_flow.exit_loop(Some(&break_label));

        assert!(result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.is_empty());
    }

    #[test]
    fn matching_continue_does_not_exit_loop() {
        let loop_label = LabelId::new("'inner");
        let continue_flow = ControlFlow::continue_to(loop_label.clone());

        let result = continue_flow.exit_loop(Some(&loop_label));

        assert!(!result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.is_empty());
    }

    #[test]
    fn non_matching_break_propagates_through_blocks() {
        let block_label = LabelId::new("'inner");
        let break_label = LabelId::new("'outer");
        let break_flow = ControlFlow::break_to(break_label.clone());

        let result = break_flow.exit_block(Some(&block_label));

        assert!(!result.can_fall_through);
        assert!(result.breaks.contains(&break_label));
        assert!(result.continues.is_empty());
    }

    #[test]
    fn body_fallthrough_does_not_exit_loop() {
        let block_label = LabelId::new("'loop");
        let fallthrough = ControlFlow::fallthrough();

        let result = fallthrough.exit_loop(Some(&block_label));

        assert!(!result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.is_empty());
    }

    #[test]
    fn break_targeting_outer_label_propagates_through_inner_loop() {
        let loop_label = LabelId::new("'inner");
        let break_label = LabelId::new("'outer");
        let break_flow = ControlFlow::break_to(break_label.clone());

        let result = break_flow.exit_loop(Some(&loop_label));

        assert!(!result.can_fall_through);
        assert!(result.breaks.contains(&break_label));
        assert!(result.continues.is_empty());
    }

    #[test]
    fn continues_targeting_outer_label_propagates_through_inner_loop() {
        let loop_label = LabelId::new("'inner");
        let continue_label = LabelId::new("'outer");
        let continue_flow = ControlFlow::continue_to(continue_label.clone());

        let result = continue_flow.exit_loop(Some(&loop_label));

        assert!(!result.can_fall_through);
        assert!(result.breaks.is_empty());
        assert!(result.continues.contains(&continue_label));
    }

    #[test]
    fn return_statement_does_not_fall_through() {
        let stmt = Stmt::Return { expr: Expr::True };

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("return statement should produce one control flow");

        assert!(!flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn break_statement_records_its_target() {
        let label = LabelId::new("'block");
        let stmt = Stmt::Break {
            label: label.clone(),
        };

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("break statement should produce one control flow");

        assert!(!flow.can_fall_through);
        assert_eq!(flow.breaks.len(), 1);
        assert!(flow.breaks.contains(&label));
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn continue_statement_records_its_target() {
        let label = LabelId::new("'block");
        let stmt = Stmt::Continue {
            label: label.clone(),
        };

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("continue statement should produce one control flow");

        assert!(!flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert_eq!(flow.continues.len(), 1);
        assert!(flow.continues.contains(&label));
    }

    #[test]
    fn expression_statement_can_fall_through() {
        let stmt = Stmt::Expr { expr: Expr::True };

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("expression statements should produce one control-flow result");

        assert!(flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn print_statement_can_fall_through() {
        let stmt = Stmt::Print { expr: Expr::True };

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("print statements should produce one control-flow result");

        assert!(flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn let_statement_can_fall_through() {
        let stmt: Stmt = crate::rust::term("let x: bool;");

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("let statements should produce one control-flow result");

        assert!(flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn empty_block_can_fall_through() {
        let (flow, _) = control_flow_block(Block::empty())
            .into_singleton()
            .expect("empty block should produce one control-flow result");

        assert!(flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn return_after_fall_through_prevents_block_fall_through() {
        let block = Block {
            label: None,
            stmts: vec![
                Stmt::Expr { expr: Expr::True },
                Stmt::Return { expr: Expr::True },
                Stmt::Break {
                    label: LabelId::new("'test"),
                },
            ],
        };

        let (flow, _) = control_flow_block(block)
            .into_singleton()
            .expect("block should produce one control-flow result");

        assert!(!flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn matching_break_makes_labeled_block_fall_through() {
        let block: Block = crate::rust::term(
            "'block: {
            break 'block;
          }",
        );

        let (flow, _) = control_flow_block(block)
            .into_singleton()
            .expect("labeled block should produce one control-flow result");

        assert!(flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn nested_block_propagates_return_flow() {
        let stmt = Stmt::Block(Block {
            label: None,
            stmts: vec![Stmt::Return { expr: Expr::True }],
        });

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("nested block should produce one control-flow result");

        assert!(!flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn if_with_two_returning_branches_does_not_fall_through() {
        let stmt: Stmt = crate::rust::term(
            " if true {
            return true;
          } else {
            return false;
          }
        ",
        );

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("if statement should produce one control-flow result");

        assert!(!flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn if_without_an_else_branch_fall_through() {
        let stmt: Stmt = crate::rust::term(
            "if true {
            return true;
        }",
        );

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("if statement should produce one control-flow result");

        assert!(flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn loop_with_fallthrough_body_does_not_fall_through() {
        let stmt: Stmt = crate::rust::term(
            "loop {
              true;
            }",
        );

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("loop statement should produce one control-flow result");

        assert!(!flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn matching_break_makes_loop_fall_through() {
        let stmt: Stmt = crate::rust::term(
            "'outer: loop {
            break 'outer;
          }",
        );

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("loop statement should produce one control-flow result");

        assert!(flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn exists_statement_propagates_inner_return() {
        let stmt: Stmt = crate::rust::term(
            "exists<'r> {
              return true;
            }",
        );

        let (flow, _) = control_flow_stmt(stmt)
            .into_singleton()
            .expect("exists statement should produce one control-flow result");

        assert!(!flow.can_fall_through);
        assert!(flow.breaks.is_empty());
        assert!(flow.continues.is_empty());
    }

    #[test]
    fn unit_returning_function_may_fall_through() {
        check_fn_returns(Ty::unit(), Block::empty())
            .into_singleton()
            .expect("unit-returning functions should be allowed to fall through");
    }

    #[test]
    fn non_unit_function_with_return_is_accepted() {
        let block: Block = crate::rust::term(
            " {
            return true;
        }",
        );

        check_fn_returns(Ty::bool(), block)
            .into_singleton()
            .expect("non-unit returning functions should not be allowed to fall through");
    }

    #[test]
    fn non_unit_function_that_can_fall_through_is_rejected() {
        check_fn_returns(Ty::bool(), Block::empty())
            .into_singleton()
            .expect_err("non-unit returning function that can fall through should be rejected.");
    }
}
