use std::{fmt::Write, ops::Deref};

use crate::{
    grammar::{
        expr::{
            Block, Expr, ExprData, FieldExpr, Init, Label, LabelId, PlaceExpr, PlaceExprData, Stmt,
        },
        Binder, Fallible, FieldName, RefKind, Ty, ValueId,
    },
    to_rust::{CodeWriter, RustBuilder},
};

impl RustBuilder {
    pub fn write_block(&mut self, out: &mut CodeWriter, block: &Block) -> Fallible<()> {
        // format!("{{ {statements} }}")
        writeln!(out, "{{")?;
        for stmt in &block.stmts {
            self.write_stmt(out, stmt)?;
            write!(out, "\n")?;
        }
        writeln!(out, "}}")?;

        Ok(())
    }

    pub fn write_stmt(&mut self, out: &mut CodeWriter, stmt: &Stmt) -> Fallible<()> {
        match stmt {
            Stmt::Let {
                label,
                id,
                ty,
                init,
            } => self.write_let(out, label.as_ref(), id, ty, init.as_ref()),
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => self.write_if(out, condition, then_block, else_block),
            Stmt::Expr { expr } => self.write_expr(out, expr),
            Stmt::Loop { label, body } => self.write_loop(out, label.as_ref(), body),
            Stmt::Break { label } => self.write_break(out, label),
            Stmt::Continue { label } => self.write_continue(out, label),
            Stmt::Return { expr } => self.write_return(out, expr),
            Stmt::Block(block) => self.write_block(out, block),
            Stmt::Exists { binder } => self.write_exists(out, binder),
        }
    }

    pub fn write_let(
        &mut self,
        out: &mut CodeWriter,
        _label: Option<&Label>,
        id: &ValueId,
        ty: &Ty,
        init: Option<&Init>,
    ) -> Fallible<()> {
        // format!("let mut {id}: {ty}{init};")
        let id = id.deref();
        let ty = self.ty_to_string(ty).unwrap();

        // TODO: Is there a way to decided if the variable should be mutable or not?
        write!(out, "let mut {id}: {ty}")?;
        if let Some(i) = init {
            write!(out, " = ")?;
            self.write_expr(out, &i.expr)?;
        }
        write!(out, ";")?;

        Ok(())
    }

    pub fn write_if(
        &mut self,
        out: &mut CodeWriter,
        condition: &Expr,
        then_block: &Block,
        else_block: &Block,
    ) -> Fallible<()> {
        // format!("if {condition} {{ {then_block} }} else {{ {else_block} }}")
        write!(out, "if ")?;
        self.write_expr(out, condition)?;
        write!(out, " {{")?;
        self.write_block(out, then_block)?;
        write!(out, " }} else {{ ")?;
        self.write_block(out, else_block)?;
        write!(out, " }}")?;

        Ok(())
    }

    pub fn write_loop(
        &mut self,
        out: &mut CodeWriter,
        label: Option<&Label>,
        body: &Block,
    ) -> Fallible<()> {
        // format!("{label}loop {{ {body} }}")
        let label = label
            .map(|l| format!("{}: ", l.id.deref()))
            .unwrap_or_default();
        write!(out, "{label}")?;
        self.write_block(out, body)
    }

    pub fn write_break(&mut self, out: &mut CodeWriter, label: &LabelId) -> Fallible<()> {
        writeln!(out, "break {}", label.deref())?;
        Ok(())
    }

    pub fn write_continue(&mut self, out: &mut CodeWriter, label: &LabelId) -> Fallible<()> {
        writeln!(out, "continue {};", label.deref())?;
        Ok(())
    }

    pub fn write_return(&mut self, out: &mut CodeWriter, expr: &Expr) -> Fallible<()> {
        writeln!(out, "return ")?;
        self.write_expr(out, expr)?;
        writeln!(out, ";")?;
        Ok(())
    }

    pub fn write_exists(&mut self, out: &mut CodeWriter, binder: &Binder<Block>) -> Fallible<()> {
        self.with_binder(binder, |term, pp| pp.write_block(out, term))
    }

    pub fn write_expr(&mut self, out: &mut CodeWriter, expr: &Expr) -> Fallible<()> {
        match expr.data() {
            ExprData::Assign { place, expr } => {
                self.write_place_expr(out, place)?;
                write!(out, " = ")?;
                self.write_expr(out, expr)?;
                writeln!(out, ";")?;
                Ok(())
            }
            ExprData::Call { callee, args } => {
                self.write_expr(out, callee)?;
                write!(out, "(")?;
                let mut sep = "";
                for arg in args {
                    write!(out, "{sep}")?;
                    self.write_expr(out, arg)?;
                    sep = ", ";
                }
                write!(out, ")")?;
                Ok(())
            }
            ExprData::Literal { value, ty } => {
                write!(out, "{value}_{}", self.scalar_to_string(ty))?;
                Ok(())
            }
            ExprData::True => {
                write!(out, "true")?;
                Ok(())
            }
            ExprData::False => {
                write!(out, "false")?;
                Ok(())
            }
            ExprData::Ref {
                kind,
                lt: _lt,
                place,
            } => {
                // format!("&{kind} {place}")
                write!(out, "&")?;

                if matches!(kind, RefKind::Mut) {
                    write!(out, "mut ")?;
                }
                self.write_place_expr(out, place)
            }
            ExprData::Place(place_expr) => self.write_place_expr(out, place_expr),
            ExprData::Turbofish { id, args } => {
                let id = id.deref();
                let args = args
                    .iter()
                    .map(|arg| self.pretty_print_parameter(arg))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap()
                    .join(", ");
                write!(out, "{id}::<{args}>")?;
                Ok(())
            }
            ExprData::Struct {
                field_exprs,
                adt_id,
                turbofish,
            } => {
                // format!("{adt_id}{turbofish} {{ {field_exprs} }}")
                let adt_id = adt_id.deref();
                let args = turbofish
                    .parameters
                    .iter()
                    .map(|arg| self.pretty_print_parameter(arg))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap()
                    .join(", ");
                let turbofish = if args.is_empty() {
                    args
                } else {
                    format!("<{args}>")
                };

                let mut sep = "";
                write!(out, "{adt_id}{turbofish} {{")?;
                for field_expr in field_exprs {
                    write!(out, "{sep}")?;
                    self.write_field_expr(out, field_expr)?;
                    sep = ", ";
                }

                Ok(())
            }
        }
    }

    pub fn write_place_expr(
        &mut self,
        out: &mut CodeWriter,
        place_expr: &PlaceExpr,
    ) -> Fallible<()> {
        match place_expr.data() {
            PlaceExprData::Var(value_id) => {
                write!(out, "{}", value_id.deref())?;
                Ok(())
            }
            PlaceExprData::Deref { prefix } => {
                write!(out, "*")?;
                self.write_place_expr(out, prefix)
            }
            PlaceExprData::Parens(place_expr) => {
                write!(out, "(")?;
                self.write_place_expr(out, place_expr)?;
                write!(out, ")")?;
                Ok(())
            }
            PlaceExprData::Field { prefix, field_name } => {
                self.write_place_expr(out, prefix)?;
                write!(out, ".{}", self.field_name_to_string(field_name))?;
                Ok(())
            }
        }
    }

    pub fn write_field_expr(
        &mut self,
        out: &mut CodeWriter,
        field_expr: &FieldExpr,
    ) -> Fallible<()> {
        match &field_expr.name {
            FieldName::Id(id) => {
                write!(out, "{}:", id.deref())?;
                self.write_expr(out, &field_expr.value)
            }
            FieldName::Index(_) => self.write_expr(out, &field_expr.value),
        }
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
            fn foo() -> u32 {
                let mut x: u32;
                return x;
            }
        );
    }
}
