//! This module defines data types that represent the surface-level
//! Rust language.
//!
//! These types are not intended for computation or semantic
//! analysis. They exist soley to translate formality code into Rust
//! code, which can then be compiled and analyzed using `rustc`.

use std::fmt::{self, Display, Formatter, Write};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RustCrate {
    pub attrs: Vec<Attr>,
    pub items: Vec<Item>,
}

impl Display for RustCrate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (index, attr) in self.attrs.iter().enumerate() {
            if index > 0 {
                writeln!(f)?;
            }
            write!(f, "{attr}")?;
        }

        if !self.attrs.is_empty() && !self.items.is_empty() {
            writeln!(f)?;
            writeln!(f)?;
        }

        for (index, item) in self.items.iter().enumerate() {
            item.fmt_pretty(f, 0)?;
            if index + 1 < self.items.len() {
                writeln!(f)?;
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attr {
    Feature(String),
}

impl Display for Attr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Attr::Feature(name) => write!(f, "#![feature({name})]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Struct(StructItem),
    Enum(EnumItem),
    Trait(TraitItem),
    Impl(ImplItem),
    NegImpl(NegImplItem),
    Function(FunctionItem),
}

impl Pretty for Item {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        match self {
            Item::Struct(item) => item.fmt_pretty(f, indent),
            Item::Enum(item) => item.fmt_pretty(f, indent),
            Item::Trait(item) => item.fmt_pretty(f, indent),
            Item::Impl(item) => item.fmt_pretty(f, indent),
            Item::NegImpl(item) => item.fmt_pretty(f, indent),
            Item::Function(item) => item.fmt_pretty(f, indent),
        }
    }
}

/// Holds the information required to print type variables and generic
/// parameters.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Generics {
    /// The generic parameters declared on items such as trait, enum
    /// and structs.  This also includes generic parameters introduced
    /// by `impl` blocks.
    ///
    /// # Example
    /// ```rust,ignore
    /// struct<T> Foo {
    ///     /* */
    /// }
    /// ```
    pub params: Vec<GenericParam>,
    pub where_clauses: Vec<WhereClause>,
}

impl Generics {
    fn fmt_params(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.params.is_empty() {
            return Ok(());
        }

        f.write_char('<')?;
        for (index, param) in self.params.iter().enumerate() {
            if index > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{param}")?;
        }
        f.write_char('>')
    }

    fn fmt_where_clauses(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.where_clauses.is_empty() {
            return Ok(());
        }

        f.write_str(" where ")?;
        for (index, wc) in self.where_clauses.iter().enumerate() {
            if index > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{wc}")?;
        }
        Ok(())
    }
}

/// Represents the generic parameters declared on an item.
///
/// # Example
/// ```rust,ignore
/// struct Foo<T> {
///     /* */
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericParam {
    Type(String),
    Lifetime(String),
    Const { name: String, ty: Type },
}

impl Display for GenericParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericParam::Type(name) => f.write_str(name),
            GenericParam::Lifetime(name) => f.write_str(name),
            GenericParam::Const { name, ty } => write!(f, "const {name}: {ty}"),
        }
    }
}

/// Represents a concrete argument supplied for a generic parameter.
///
/// # Example
/// ```rust,ignore
/// let foo = Foo::<u32> { /* */ }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericArg {
    Type(Type),
    Lifetime(String),
    Const(ConstExpr),
}

impl Display for GenericArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericArg::Type(ty) => write!(f, "{ty}"),
            GenericArg::Lifetime(lt) => f.write_str(lt),
            GenericArg::Const(konst) => write!(f, "{konst}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstExpr {
    Ident(String),
    Scalar { value: String, suffix: String },
}

impl Display for ConstExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ConstExpr::Ident(name) => f.write_str(name),
            ConstExpr::Scalar { value, suffix } => write!(f, "{value}_{suffix}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A named type or type path, with generic arguments.
    /// (e.g., `Vec<T>`)
    Path {
        name: String,
        args: Vec<GenericArg>,
    },
    Ref {
        lifetime: Option<String>,
        mutable: bool,
        ty: Box<Type>,
    },
    RawPtr {
        mutable: bool,
        ty: Box<Type>,
    },
    Tuple(Vec<Type>),
    FnPtr {
        inputs: Vec<Type>,
        output: Box<Type>,
    },
    Never,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Path { name, args } => {
                f.write_str(name)?;
                fmt_generic_args(f, args, false)
            }
            Type::Ref {
                lifetime,
                mutable,
                ty,
            } => {
                f.write_char('&')?;
                if let Some(lt) = lifetime {
                    write!(f, "{lt} ")?;
                }
                if *mutable {
                    f.write_str("mut ")?;
                }
                write!(f, "{ty}")
            }
            Type::RawPtr { mutable, ty } => {
                if *mutable {
                    write!(f, "*mut {ty}")
                } else {
                    write!(f, "*const {ty}")
                }
            }
            Type::Tuple(types) => {
                f.write_char('(')?;
                for (index, ty) in types.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                if types.len() == 1 {
                    f.write_char(',')?;
                }
                f.write_char(')')
            }
            Type::FnPtr { inputs, output } => {
                f.write_str("fn(")?;
                for (index, ty) in inputs.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ") -> {output}")
            }
            Type::Never => f.write_char('!'),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhereClause {
    Trait {
        ty: Type,
        trait_name: String,
        args: Vec<GenericArg>,
    },
}

impl Display for WhereClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            WhereClause::Trait {
                ty,
                trait_name,
                args,
            } => {
                write!(f, "{ty}: {trait_name}")?;
                fmt_generic_args(f, args, false)
            }
        }
    }
}

/// Type bounds used for an associated type in a trait. See [].
/// Represents a type bound on an associated type declared withing a
/// trait.
///
/// See [`AssociatedTypeItem`] for how these bounds are used in trait
/// definitions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeBound {
    Trait {
        trait_name: String,
        args: Vec<GenericArg>,
    },
}

impl Display for TypeBound {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeBound::Trait { trait_name, args } => {
                write!(f, "{trait_name}")?;
                fmt_generic_args(f, args, false)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructItem {
    pub name: String,
    pub generics: Generics,
    pub fields: Vec<StructField>,
}

impl Pretty for StructItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        write!(f, "struct {}", self.name)?;
        self.generics.fmt_params(f)?;
        self.generics.fmt_where_clauses(f)?;
        f.write_str(" {")?;

        if !self.fields.is_empty() {
            writeln!(f)?;
            for field in &self.fields {
                write_indent(f, indent + 1)?;
                writeln!(f, "{}: {},", field.name, field.ty)?;
            }
            write_indent(f, indent)?;
        }

        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumItem {
    pub name: String,
    pub generics: Generics,
    pub variants: Vec<EnumVariant>,
}

impl Pretty for EnumItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        write!(f, "enum {}", self.name)?;
        self.generics.fmt_params(f)?;
        self.generics.fmt_where_clauses(f)?;
        f.write_str(" {")?;

        if !self.variants.is_empty() {
            writeln!(f)?;
            for variant in &self.variants {
                write_indent(f, indent + 1)?;
                variant.fmt_inline(f)?;
                writeln!(f, ",")?;
            }
            write_indent(f, indent)?;
        }

        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: VariantFields,
}

impl EnumVariant {
    fn fmt_inline(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)?;
        match &self.fields {
            VariantFields::Unit => Ok(()),
            VariantFields::Tuple(fields) => {
                f.write_char('(')?;
                for (index, ty) in fields.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                f.write_char(')')
            }
            VariantFields::Struct(fields) => {
                f.write_str(" { ")?;
                for (index, field) in fields.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}: {}", field.name, field.ty)?;
                }
                f.write_str(" }")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariantFields {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionBody {
    Declaration,
    /// Generated by the formality code `{ trusted }`
    Trusted,
    Block(Block),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionItem {
    pub is_unsafe: bool,
    pub name: String,
    pub generics: Generics,
    pub params: Vec<FnParam>,
    pub return_ty: Type,
    pub body: FunctionBody,
}

impl Pretty for FunctionItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        if self.is_unsafe {
            f.write_str("unsafe ")?;
        }

        write!(f, "fn {}", self.name)?;
        self.generics.fmt_params(f)?;

        f.write_char('(')?;
        for (index, param) in self.params.iter().enumerate() {
            if index > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{}: {}", param.name, param.ty)?;
        }
        write!(f, ") -> {}", self.return_ty)?;
        self.generics.fmt_where_clauses(f)?;

        match &self.body {
            FunctionBody::Declaration => f.write_char(';'),
            FunctionBody::Trusted => {
                f.write_str(" {")?;
                writeln!(f)?;
                write_indent(f, indent + 1)?;
                writeln!(f, "panic!(\"Trusted Fn Body\")")?;
                write_indent(f, indent)?;
                f.write_char('}')
            }
            FunctionBody::Block(block) => {
                f.write_char(' ')?;
                block.fmt_pretty(f, indent)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitItem {
    pub is_unsafe: bool,
    pub name: String,
    pub generics: Generics,
    pub items: Vec<TraitMember>,
}

impl Pretty for TraitItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        if self.is_unsafe {
            f.write_str("unsafe ")?;
        }

        write!(f, "trait {}", self.name)?;
        self.generics.fmt_params(f)?;
        self.generics.fmt_where_clauses(f)?;

        if self.items.is_empty() {
            return f.write_str(" { }");
        }

        f.write_str(" {")?;
        writeln!(f)?;
        for member in &self.items {
            member.fmt_pretty(f, indent + 1)?;
            writeln!(f)?;
        }
        write_indent(f, indent)?;
        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraitMember {
    Function(FunctionItem),
    AssociatedType(AssociatedTypeItem),
}

impl Pretty for TraitMember {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        match self {
            TraitMember::Function(function) => function.fmt_pretty(f, indent),
            TraitMember::AssociatedType(assoc_ty) => assoc_ty.fmt_pretty(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssociatedTypeItem {
    pub name: String,
    pub generics: Generics,
    pub bounds: Vec<TypeBound>,
}

impl Pretty for AssociatedTypeItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        write!(f, "type {}", self.name)?;
        self.generics.fmt_params(f)?;

        if !self.bounds.is_empty() {
            f.write_str(": ")?;
            for (index, bound) in self.bounds.iter().enumerate() {
                if index > 0 {
                    f.write_str(" + ")?;
                }
                write!(f, "{bound}")?;
            }
        }

        self.generics.fmt_where_clauses(f)?;
        f.write_char(';')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplItem {
    pub is_unsafe: bool,
    pub generics: Generics,
    pub trait_name: String,
    /// Generic arguments supplied to the trait implementation.
    ///
    /// These may include concrete types, lifetimes, constants, as
    /// well as generic parameters introduced by the `impl` itself.
    /// Generic parameters used here are recorded in the
    /// `generics` field as well.
    pub trait_args: Vec<GenericArg>,
    pub self_ty: Type,
    pub items: Vec<ImplMember>,
}

impl Pretty for ImplItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        if self.is_unsafe {
            f.write_str("unsafe ")?;
        }

        f.write_str("impl")?;
        self.generics.fmt_params(f)?;
        write!(f, " {}", self.trait_name)?;
        fmt_generic_args(f, &self.trait_args, false)?;
        write!(f, " for {}", self.self_ty)?;
        self.generics.fmt_where_clauses(f)?;
        f.write_str(" {")?;

        if !self.items.is_empty() {
            writeln!(f)?;
            for item in &self.items {
                item.fmt_pretty(f, indent + 1)?;
                writeln!(f)?;
            }
            write_indent(f, indent)?;
        }

        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplMember {
    Function(FunctionItem),
    AssociatedTypeValue(AssociatedTypeValueItem),
}

impl Pretty for ImplMember {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        match self {
            ImplMember::Function(function) => function.fmt_pretty(f, indent),
            ImplMember::AssociatedTypeValue(value) => value.fmt_pretty(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssociatedTypeValueItem {
    pub name: String,
    pub generics: Generics,
    pub ty: Type,
}

impl Pretty for AssociatedTypeValueItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        write!(f, "type {}", self.name)?;
        self.generics.fmt_params(f)?;
        write!(f, " = {}", self.ty)?;
        self.generics.fmt_where_clauses(f)?;
        f.write_char(';')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NegImplItem {
    pub is_unsafe: bool,
    pub generics: Generics,
    pub trait_name: String,
    pub trait_args: Vec<GenericArg>,
    pub self_ty: Type,
    pub where_clauses: Vec<WhereClause>,
}

impl Pretty for NegImplItem {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;
        if self.is_unsafe {
            f.write_str("unsafe ")?;
        }

        f.write_str("impl")?;
        self.generics.fmt_params(f)?;
        write!(f, " !{}", self.trait_name)?;
        fmt_generic_args(f, &self.trait_args, false)?;
        write!(f, " for {}", self.self_ty)?;

        if !self.where_clauses.is_empty() {
            f.write_str(" where ")?;
            for (index, wc) in self.where_clauses.iter().enumerate() {
                if index > 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{wc}")?;
            }
        }

        f.write_str(" {}")
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Pretty for Block {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        f.write_char('{')?;

        if !self.stmts.is_empty() {
            writeln!(f)?;
            for stmt in &self.stmts {
                stmt.fmt_pretty(f, indent + 1)?;
                writeln!(f)?;
            }
            write_indent(f, indent)?;
        }

        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Let {
        mutable: bool,
        name: String,
        ty: Type,
        init: Option<Expr>,
    },
    If {
        condition: Expr,
        then_block: Block,
        else_block: Block,
    },
    Expr(Expr),
    Loop {
        label: Option<String>,
        body: Block,
    },
    Break {
        label: String,
    },
    Continue {
        label: String,
    },
    Return {
        expr: Expr,
    },
    Block(Block),
}

impl Pretty for Stmt {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        write_indent(f, indent)?;

        match self {
            Stmt::Let {
                mutable,
                name,
                ty,
                init,
            } => {
                f.write_str("let ")?;
                if *mutable {
                    f.write_str("mut ")?;
                }
                write!(f, "{name}: {ty}")?;
                if let Some(init) = init {
                    write!(f, " = {init}")?;
                }
                f.write_char(';')
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                write!(f, "if {condition} ")?;
                then_block.fmt_pretty(f, indent)?;
                f.write_str(" else ")?;
                else_block.fmt_pretty(f, indent)
            }
            Stmt::Expr(expr) => write!(f, "{expr};"),
            Stmt::Loop { label, body } => {
                if let Some(label) = label {
                    write!(f, "{label}: ")?;
                }
                f.write_str("loop ")?;
                body.fmt_pretty(f, indent)
            }
            Stmt::Break { label } => write!(f, "break {label};"),
            Stmt::Continue { label } => write!(f, "continue {label};"),
            Stmt::Return { expr } => write!(f, "return {expr};"),
            Stmt::Block(block) => block.fmt_pretty(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Assign {
        place: PlaceExpr,
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Literal {
        value: String,
        suffix: String,
    },
    Bool(bool),
    Ref {
        mutable: bool,
        place: PlaceExpr,
    },
    Place(PlaceExpr),
    Path {
        name: String,
        args: Vec<GenericArg>,
    },
    Struct {
        path: String,
        args: Vec<GenericArg>,
        fields: StructExprFields,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Assign { place, value } => write!(f, "{place} = {value}"),
            Expr::Call { callee, args } => {
                write!(f, "{callee}(")?;
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                f.write_char(')')
            }
            Expr::Literal { value, suffix } => write!(f, "{value}_{suffix}"),
            Expr::Bool(true) => f.write_str("true"),
            Expr::Bool(false) => f.write_str("false"),
            Expr::Ref { mutable, place } => {
                f.write_char('&')?;
                if *mutable {
                    f.write_str("mut ")?;
                }
                write!(f, "{place}")
            }
            Expr::Place(place) => write!(f, "{place}"),
            Expr::Path { name, args } => {
                f.write_str(name)?;
                fmt_generic_args(f, args, true)
            }
            Expr::Struct { path, args, fields } => {
                f.write_str(path)?;
                fmt_generic_args(f, args, true)?;

                match fields {
                    StructExprFields::Named(fields) => {
                        f.write_str(" { ")?;
                        for (index, field) in fields.iter().enumerate() {
                            if index > 0 {
                                f.write_str(", ")?;
                            }
                            write!(f, "{}: {}", field.name, field.expr)?;
                        }
                        f.write_str(" }")
                    }
                    StructExprFields::Tuple(fields) => {
                        f.write_char('(')?;
                        for (index, expr) in fields.iter().enumerate() {
                            if index > 0 {
                                f.write_str(", ")?;
                            }
                            write!(f, "{expr}")?;
                        }
                        f.write_char(')')
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructExprFields {
    Named(Vec<NamedFieldExpr>),
    Tuple(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedFieldExpr {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlaceExpr {
    Var(String),
    Deref(Box<PlaceExpr>),
    Paren(Box<PlaceExpr>),
    Field {
        prefix: Box<PlaceExpr>,
        field: String,
    },
}

impl Display for PlaceExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PlaceExpr::Var(name) => f.write_str(name),
            PlaceExpr::Deref(prefix) => write!(f, "*{prefix}"),
            PlaceExpr::Paren(place) => write!(f, "({place})"),
            PlaceExpr::Field { prefix, field } => write!(f, "{prefix}.{field}"),
        }
    }
}

trait Pretty {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result;
}

fn fmt_generic_args(f: &mut Formatter<'_>, args: &[GenericArg], turbofish: bool) -> fmt::Result {
    if args.is_empty() {
        return Ok(());
    }

    if turbofish {
        f.write_str("::<")?;
    } else {
        f.write_char('<')?;
    }

    for (index, arg) in args.iter().enumerate() {
        if index > 0 {
            f.write_str(", ")?;
        }
        write!(f, "{arg}")?;
    }

    f.write_char('>')
}

const INDENT: &str = "    ";

fn write_indent(f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
    for _ in 0..indent {
        f.write_str(INDENT)?;
    }
    Ok(())
}
