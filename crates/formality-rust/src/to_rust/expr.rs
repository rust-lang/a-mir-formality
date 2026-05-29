use std::ops::Deref;

use crate::grammar::{
    expr::{Block, Expr, FieldExpr, Init, Label, PlaceExpr, Stmt},
    Binder, Fallible, FieldName, RefKind, Ty, ValueId,
};

use crate::to_rust::{context::Context, syntax, tys};

pub fn lower_block(ctx: &mut Context, block: &Block) -> Fallible<syntax::Block> {
    let label = block.label.as_ref().map(|l| l.id.as_str().to_owned());
    let stmts = block
        .stmts
        .iter()
        .map(|stmt| lower_stmt(ctx, stmt))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(syntax::Block { label, stmts })
}

pub fn lower_stmt(ctx: &mut Context, stmt: &Stmt) -> Fallible<syntax::Stmt> {
    match stmt {
        Stmt::Let {
            label,
            id,
            ty,
            init,
        } => lower_let(ctx, label.as_ref(), id, ty, init.as_ref()),
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => Ok(syntax::Stmt::If {
            condition: lower_expr(ctx, condition)?,
            then_block: lower_block(ctx, then_block)?,
            else_block: lower_block(ctx, else_block)?,
        }),
        Stmt::Expr { expr } => Ok(syntax::Stmt::Expr(lower_expr(ctx, expr)?)),
        Stmt::Loop { label, body } => Ok(syntax::Stmt::Loop {
            label: label.as_ref().map(|l| l.id.deref().clone()),
            body: lower_block(ctx, body)?,
        }),
        Stmt::Break { label } => Ok(syntax::Stmt::Break {
            label: label.deref().clone(),
        }),
        Stmt::Continue { label } => Ok(syntax::Stmt::Continue {
            label: label.deref().clone(),
        }),
        Stmt::Return { expr } => Ok(syntax::Stmt::Return {
            expr: lower_expr(ctx, expr)?,
        }),
        Stmt::Block(block) => Ok(syntax::Stmt::Block(lower_block(ctx, block)?)),
        Stmt::Exists { binder } => lower_exists_stmt(ctx, binder),
        Stmt::Print { expr } => Ok(syntax::Stmt::Expr(lower_expr(ctx, expr)?)),
    }
}

pub fn lower_let(
    ctx: &mut Context,
    _label: Option<&Label>,
    id: &ValueId,
    ty: &Ty,
    init: Option<&Init>,
) -> Fallible<syntax::Stmt> {
    Ok(syntax::Stmt::Let {
        // TODO: is there a way to know, if the variable must be mutable or not?
        mutable: true,
        name: id.deref().clone(),
        ty: tys::lower_ty(ctx, ty)?,
        init: init.map(|init| lower_expr(ctx, &init.expr)).transpose()?,
    })
}

pub fn lower_exists_stmt(ctx: &mut Context, binder: &Binder<Block>) -> Fallible<syntax::Stmt> {
    // TODO: Check again, after codegen is merged, if "erased" lifetimes could work here.
    let (term, _) = ctx.open_exists(binder);
    let result = syntax::Stmt::Block(lower_block(ctx, &term)?);
    Ok(result)
}

pub fn lower_expr(ctx: &mut Context, expr: &Expr) -> Fallible<syntax::Expr> {
    match expr {
        Expr::Assign { place, expr } => Ok(syntax::Expr::Assign {
            place: lower_place_expr(ctx, place)?,
            value: Box::new(lower_expr(ctx, expr)?),
        }),
        Expr::Call { callee, args } => Ok(syntax::Expr::Call {
            callee: Box::new(lower_expr(ctx, callee)?),
            args: args
                .iter()
                .map(|arg| lower_expr(ctx, arg))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        Expr::Literal { value, ty } => Ok(syntax::Expr::Literal {
            value: value.to_string(),
            suffix: tys::scalar_to_string(ty),
        }),
        Expr::True => Ok(syntax::Expr::Bool(true)),
        Expr::False => Ok(syntax::Expr::Bool(false)),
        Expr::Ref { kind, lt: _, place } => Ok(syntax::Expr::Ref {
            mutable: matches!(kind, RefKind::Mut),
            place: lower_place_expr(ctx, place)?,
        }),
        Expr::Place(place_expr) => Ok(syntax::Expr::Place(lower_place_expr(ctx, place_expr)?)),
        Expr::Turbofish { id, args } => Ok(syntax::Expr::Path {
            name: id.deref().clone(),
            args: args
                .iter()
                .map(|arg| tys::lower_generic_arg(ctx, arg))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        Expr::Struct {
            field_exprs,
            adt_id,
            turbofish,
        } => {
            let args = turbofish
                .parameters
                .iter()
                .map(|arg| tys::lower_generic_arg(ctx, arg))
                .collect::<Result<Vec<_>, _>>()?;

            let named_fields = field_exprs
                .iter()
                .filter_map(|field| match field.name {
                    FieldName::Id(_) => Some(lower_named_field_expr(ctx, field)),
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
                .map(|field| lower_expr(ctx, &field.value))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(syntax::Expr::Struct {
                path: adt_id.deref().clone(),
                args,
                fields: syntax::StructExprFields::Tuple(tuple_fields),
            })
        }
    }
}

pub fn lower_place_expr(ctx: &mut Context, place_expr: &PlaceExpr) -> Fallible<syntax::PlaceExpr> {
    match place_expr {
        PlaceExpr::Var(value_id) => Ok(syntax::PlaceExpr::Var(value_id.deref().clone())),
        PlaceExpr::Deref { prefix } => Ok(syntax::PlaceExpr::Deref(Box::new(lower_place_expr(
            ctx, prefix,
        )?))),
        PlaceExpr::Parens(place_expr) => Ok(syntax::PlaceExpr::Paren(Box::new(lower_place_expr(
            ctx, place_expr,
        )?))),
        PlaceExpr::Field { prefix, field_name } => Ok(syntax::PlaceExpr::Field {
            prefix: Box::new(lower_place_expr(ctx, prefix)?),
            field: match field_name {
                FieldName::Id(id) => id.deref().clone(),
                FieldName::Index(idx) => idx.to_string(),
            },
        }),
    }
}

pub fn lower_named_field_expr(
    ctx: &mut Context,
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
        expr: lower_expr(ctx, &field_expr.value)?,
    })
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
pub fn foo() -> u32 {
    let mut x: u32;
    return x;
}"#
        );
    }

    #[test]
    fn fn_with_named_block() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn foo() -> () {
                        'a: {
                            break 'a;
                        }
                    }
                }
            ],
            r#"
pub fn foo() -> () {
    'a: {
        break 'a;
    }
}"#
        );
    }

    #[test]
    fn omited_existential_lifetimes() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn foo() -> u32 {
                        exists<'r0, 'r1> {
                            let v1: u32 = 0 _ u32;
                            let v2: &mut 'r0 u32 = &mut 'r1 v1;
                            return *v2;
                        }
                    }
                }
            ],
            r#"
pub fn foo() -> u32 {
    {
        let mut v1: u32 = 0_u32;
        let mut v2: &mut u32 = &mut v1;
        return *v2;
    }
}
"#
        );
    }
}
