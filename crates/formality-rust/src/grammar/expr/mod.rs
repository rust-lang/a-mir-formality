mod codegen;

use std::sync::Arc;

use formality_core::{cast_impl, id, term, DowncastTo, UpcastFrom};

use crate::grammar::{
    AdtId, Binder, FieldName, Lt, Parameter, RefKind, ScalarId, TraitId, Ty, ValueId,
};

id!(LabelId, regex = "'[a-zA-Z_][a-zA-Z0-9_]*");

#[term($id :)]
pub struct Label {
    pub id: LabelId,
}

#[term($?label { $*stmts })]
pub struct Block {
    pub label: Option<Label>,
    pub stmts: Vec<Stmt>,
}

/// An optional initializer expression, parsed as `= $expr`.
#[term(= $expr)]
pub struct Init {
    pub expr: Expr,
}

/// A statement within a block.
#[term]
pub enum Stmt {
    /// `let id: ty = expr;` or `let 'a: id: ty = expr;` or `let id: ty;`
    ///
    /// Bind a variable, optionally with an initializer.
    /// If no label is given, the variable is bound in the innermost
    /// enclosing block. If a label is given, the variable is bound
    /// in the named block and dropped when that block exits.
    /// If no initializer is given, the variable is uninitialized
    /// and must be assigned before use.
    #[grammar(let $?label $id : $ty $?init ;)]
    Let {
        label: Option<Label>,
        id: ValueId,
        ty: Ty,
        init: Option<Init>,
    },

    /// `if condition { then } else { else }`
    ///
    /// Conditional statement. Both branches are required.
    #[grammar(if $condition $then_block else $else_block)]
    If {
        condition: Expr,
        then_block: Block,
        else_block: Block,
    },

    /// `expr;`
    ///
    /// Evaluate an expression as a statement (discards the result).
    #[grammar($expr ;)]
    Expr { expr: Expr },

    /// `label: loop { body }` or `loop { body }`
    ///
    /// A loop. The optional label can be targeted by break/continue.
    #[grammar($?label loop $body)]
    Loop { label: Option<Label>, body: Block },

    /// `break label;`
    ///
    /// Break out of the labeled block or loop.
    #[grammar(break $label ;)]
    Break { label: LabelId },

    /// `continue label;`
    ///
    /// Jump to the top of the labeled loop.
    #[grammar(continue $label ;)]
    Continue { label: LabelId },

    /// `return expr;`
    ///
    /// Return from the current function.
    #[grammar(return $expr ;)]
    Return { expr: Expr },

    /// `{ stmts }`
    ///
    /// A nested block. Creates a new scope; locals declared
    /// inside are dropped when the block exits.
    #[cast]
    Block(Block),

    /// `exists<'a, 'b> { stmts }`
    ///
    /// Introduce existential variables (e.g., lifetime inference variables)
    /// over a block of statements.
    #[grammar(exists $binder)]
    Exists { binder: Binder<Block> },
}

#[term($data)]
pub struct Expr {
    pub data: Arc<ExprData>,
}

impl Expr {
    pub fn data(&self) -> &ExprData {
        &self.data
    }
}

#[term]
pub enum ExprData {
    /// `place = expr`
    ///
    /// Assign a value to a place expression. Evaluates to `()`.
    /// The left-hand side must be a place expression (variable, deref, field).
    #[grammar($place = $expr)]
    Assign { place: PlaceExpr, expr: Expr },

    /// `call callee<tys>(args)`
    ///
    /// Call a function. The callee is a named function reference.
    /// Returns the call's result as a value.
    #[grammar($callee ($,args))]
    Call { callee: Expr, args: Vec<Expr> },

    /// `42_u32`
    ///
    /// A scalar literal.
    #[grammar($value _ $ty)]
    Literal { value: usize, ty: ScalarId },

    /// `true`
    #[grammar(true)]
    True,

    /// `false`
    #[grammar(false)]
    False,

    /// `& expr` or `& mut expr`
    ///
    /// Create a reference. The operand must be a place expression.
    #[grammar(& $?kind $lt $place)]
    Ref {
        kind: RefKind,
        lt: Lt,
        place: PlaceExpr,
    },

    /// A place expression used as a value.
    #[cast]
    Place(PlaceExpr),

    #[grammar($id::<$,args>)]
    Turbofish { id: ValueId, args: Vec<Parameter> },

    /// `$adt_id { exprs }`
    ///
    /// Construct an aggregate (struct) value.
    #[grammar($adt_id $?turbofish { $,field_exprs })]
    Struct {
        field_exprs: Vec<FieldExpr>,
        adt_id: AdtId,
        turbofish: Turbofish,
    },
}

impl DowncastTo<ExprData> for Expr {
    fn downcast_to(&self) -> Option<ExprData> {
        Some(ExprData::clone(&self.data))
    }
}

#[term($data)]
pub struct PlaceExpr {
    pub data: Arc<PlaceExprData>,
}

impl PlaceExpr {
    pub fn data(&self) -> &PlaceExprData {
        &self.data
    }

    pub fn is_prefix_of(&self, other: &PlaceExpr) -> bool {
        other.all_prefixes().contains(&self)
    }

    pub fn all_prefixes(&self) -> Vec<&PlaceExpr> {
        let mut result = vec![self];
        let mut current = self;
        while let Some(prefix) = current.prefix() {
            result.push(prefix);
            current = prefix;
        }
        result
    }

    pub fn prefix(&self) -> Option<&PlaceExpr> {
        match self.data() {
            PlaceExprData::Var(_) => None,
            PlaceExprData::Deref { prefix } => Some(prefix),
            PlaceExprData::Field {
                prefix,
                field_name: _,
            } => Some(prefix),
            PlaceExprData::Parens(place_expr) => Some(place_expr),
        }
    }
}

impl UpcastFrom<PlaceExprData> for PlaceExpr {
    fn upcast_from(data: PlaceExprData) -> Self {
        PlaceExpr {
            data: Arc::new(data),
        }
    }
}

impl DowncastTo<PlaceExprData> for PlaceExpr {
    fn downcast_to(&self) -> Option<PlaceExprData> {
        Some(PlaceExprData::clone(&self.data))
    }
}

// ANCHOR: PlaceExprData
#[term]
pub enum PlaceExprData {
    /// `x`
    ///
    /// A variable reference. Whether this is a place (lvalue) or
    /// value (rvalue) depends on context: place on the left of `=`
    /// or as operand of `&`, value everywhere else.
    #[cast]
    Var(ValueId),

    /// `* expr`
    ///
    /// Dereference. Like `Var`, place vs value depends on context.
    #[grammar(* $prefix)]
    Deref { prefix: PlaceExpr },

    /// `( expr )`
    ///
    /// Parenthesized expression, needed so users can write `(*x).field`.
    #[grammar(($v0))]
    Parens(PlaceExpr),

    /// `expr . field`
    ///
    /// Field projection. Like `Var`, place vs value depends on context.
    #[grammar($prefix . $field_name)]
    #[reject(PlaceExprData::Deref { .. }, _)]
    Field {
        prefix: PlaceExpr,
        field_name: FieldName,
    },
}
// ANCHOR_END: PlaceExprData

cast_impl!((ValueId) <: (PlaceExprData) <: (PlaceExpr));

/// A named reference to a function or associated function.
#[term($name: $value)]
pub struct FieldExpr {
    pub name: FieldName,
    pub value: Expr,
}

#[term(::$<parameters>)]
#[derive(Default)]
pub struct Turbofish {
    pub parameters: Vec<Parameter>,
}

/// A named reference to a function or associated function.
#[term]
pub enum FnName {
    #[grammar($v0)]
    FreeId(ValueId),

    #[grammar(<$ty as $trait_id>::$id)]
    QualifiedId {
        ty: Ty,
        trait_id: TraitId,
        id: ValueId,
    },
}
