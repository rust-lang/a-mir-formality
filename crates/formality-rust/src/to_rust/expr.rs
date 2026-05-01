use std::ops::Deref;

use crate::grammar::{
    expr::{Block, Expr, ExprData, FieldExpr, Init, Label, PlaceExpr, PlaceExprData, Stmt},
    Binder, Fallible, FieldName, RefKind, Ty, ValueId,
};

use super::{syntax, RustBuilder};

impl RustBuilder {
    pub fn lower_block(&mut self, block: &Block) -> Fallible<syntax::Block> {
        let stmts = block
            .stmts
            .iter()
            .map(|stmt| self.lower_stmt(stmt))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(syntax::Block { stmts })
    }

    fn lower_stmt(&mut self, stmt: &Stmt) -> Fallible<syntax::Stmt> {
        match stmt {
            Stmt::Let {
                label,
                id,
                ty,
                init,
            } => self.lower_let(label.as_ref(), id, ty, init.as_ref()),
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => Ok(syntax::Stmt::If {
                condition: self.lower_expr(condition)?,
                then_block: self.lower_block(then_block)?,
                else_block: self.lower_block(else_block)?,
            }),
            Stmt::Expr { expr } => Ok(syntax::Stmt::Expr(self.lower_expr(expr)?)),
            Stmt::Loop { label, body } => Ok(syntax::Stmt::Loop {
                label: label.as_ref().map(|l| l.id.deref().clone()),
                body: self.lower_block(body)?,
            }),
            Stmt::Break { label } => Ok(syntax::Stmt::Break {
                label: label.deref().clone(),
            }),
            Stmt::Continue { label } => Ok(syntax::Stmt::Continue {
                label: label.deref().clone(),
            }),
            Stmt::Return { expr } => Ok(syntax::Stmt::Return {
                expr: self.lower_expr(expr)?,
            }),
            Stmt::Block(block) => Ok(syntax::Stmt::Block(self.lower_block(block)?)),
            Stmt::Exists { binder } => self.lower_exists_stmt(binder),
            Stmt::Print { expr } => Ok(syntax::Stmt::Expr(self.lower_expr(expr)?)),
        }
    }

    fn lower_let(
        &mut self,
        _label: Option<&Label>,
        id: &ValueId,
        ty: &Ty,
        init: Option<&Init>,
    ) -> Fallible<syntax::Stmt> {
        Ok(syntax::Stmt::Let {
            // TODO: is there a way to knwo, if the variable must be mutable or not?
            mutable: true,
            name: id.deref().clone(),
            ty: self.lower_ty(ty)?,
            init: init.map(|init| self.lower_expr(&init.expr)).transpose()?,
        })
    }

    fn lower_exists_stmt(&mut self, binder: &Binder<Block>) -> Fallible<syntax::Stmt> {
        self.with_binder(binder, |term, pp| {
            Ok(syntax::Stmt::Block(pp.lower_block(term)?))
        })
    }

    pub fn lower_expr(&mut self, expr: &Expr) -> Fallible<syntax::Expr> {
        match expr.data() {
            ExprData::Assign { place, expr } => Ok(syntax::Expr::Assign {
                place: self.lower_place_expr(place)?,
                value: Box::new(self.lower_expr(expr)?),
            }),
            ExprData::Call { callee, args } => Ok(syntax::Expr::Call {
                callee: Box::new(self.lower_expr(callee)?),
                args: args
                    .iter()
                    .map(|arg| self.lower_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            ExprData::Literal { value, ty } => Ok(syntax::Expr::Literal {
                value: value.to_string(),
                suffix: self.scalar_to_string(ty),
            }),
            ExprData::True => Ok(syntax::Expr::Bool(true)),
            ExprData::False => Ok(syntax::Expr::Bool(false)),
            ExprData::Ref { kind, lt: _, place } => Ok(syntax::Expr::Ref {
                mutable: matches!(kind, RefKind::Mut),
                place: self.lower_place_expr(place)?,
            }),
            ExprData::Place(place_expr) => {
                Ok(syntax::Expr::Place(self.lower_place_expr(place_expr)?))
            }
            ExprData::Turbofish { id, args } => Ok(syntax::Expr::Path {
                name: id.deref().clone(),
                args: args
                    .iter()
                    .map(|arg| self.lower_generic_arg(arg))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            ExprData::Struct {
                field_exprs,
                adt_id,
                turbofish,
            } => {
                let args = turbofish
                    .parameters
                    .iter()
                    .map(|arg| self.lower_generic_arg(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                let named_fields = field_exprs
                    .iter()
                    .filter_map(|field| match field.name {
                        FieldName::Id(_) => Some(self.lower_named_field_expr(field)),
                        FieldName::Index(_) => None,
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if named_fields.len() == field_exprs.len() {
                    return Ok(syntax::Expr::Struct {
                        path: adt_id.deref().clone(),
                        args,
                        fields: syntax::StructExprFields::Named(named_fields),
                    });
                }

                let tuple_fields = field_exprs
                    .iter()
                    .map(|field| self.lower_expr(&field.value))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(syntax::Expr::Struct {
                    path: adt_id.deref().clone(),
                    args,
                    fields: syntax::StructExprFields::Tuple(tuple_fields),
                })
            }
        }
    }

    pub fn lower_place_expr(&mut self, place_expr: &PlaceExpr) -> Fallible<syntax::PlaceExpr> {
        match place_expr.data() {
            PlaceExprData::Var(value_id) => Ok(syntax::PlaceExpr::Var(value_id.deref().clone())),
            PlaceExprData::Deref { prefix } => Ok(syntax::PlaceExpr::Deref(Box::new(
                self.lower_place_expr(prefix)?,
            ))),
            PlaceExprData::Parens(place_expr) => Ok(syntax::PlaceExpr::Paren(Box::new(
                self.lower_place_expr(place_expr)?,
            ))),
            PlaceExprData::Field { prefix, field_name } => Ok(syntax::PlaceExpr::Field {
                prefix: Box::new(self.lower_place_expr(prefix)?),
                field: match field_name {
                    FieldName::Id(id) => id.deref().clone(),
                    FieldName::Index(idx) => idx.to_string(),
                },
            }),
        }
    }

    fn lower_named_field_expr(
        &mut self,
        field_expr: &FieldExpr,
    ) -> Fallible<syntax::NamedFieldExpr> {
        let name = match &field_expr.name {
            FieldName::Id(id) => id.deref().clone(),
            FieldName::Index(_) => {
                anyhow::bail!("expected named field expression but found tuple field")
            }
        };

        Ok(syntax::NamedFieldExpr {
            name,
            expr: self.lower_expr(&field_expr.value)?,
        })
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn simple_fn_body() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn foo() -> u32 {
                        let x: u32;
                        return x;
                    }
                }
            ],
            r#"
fn foo() -> u32 {
    let mut x: u32;
    return x;
}
"#
        );
    }
}
