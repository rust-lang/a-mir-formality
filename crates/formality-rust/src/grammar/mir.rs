use super::VariantId;
use formality_macros::term;
use formality_types::{
    cast::{Downcast, DowncastFrom, Upcast},
    grammar::{AdtId, Binder, FieldId, FnId, Lt, Parameter, RefKind, Ty},
};

#[term(mir($binder))]
pub struct MirFnBody {
    /// The binder binds existential lifetimes that appear in types
    /// but whose values are not computed by earlier phases.
    pub binder: Binder<LocalsAndBlocks>,
}

#[term]
pub struct LocalsAndBlocks {
    pub local_decls: Vec<LocalDecl>,
    pub basic_block_decls: Vec<BasicBlockDecl>,
}

#[term(($mutability $name: $ty))]
pub struct LocalDecl {
    pub name: LocalId,
    pub ty: Ty,
    pub mutability: RefKind,
}

formality_types::id!(LocalId);
formality_types::id!(BasicBlockId);

#[term]
pub struct BasicBlockDecl {
    pub id: BasicBlockId,
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[term]
pub enum Statement {
    #[grammar(($v0 = $v1))]
    Assign(Place, Rvalue),
    // SetDiscriminant(Place, VariantId),
    Noop,
    FakeRead(Place),
}

#[term]
pub enum Rvalue {
    Use(Operand),
    Repeat(Operand, Constant),
    Ref(Lt, RefKind, Place),
    AddrOf(RefKind, Place),
    Len(Place),
    Apply(Operand, BinaryOp, Operand),
    Checked(Operand, BinaryOp, Operand),
    Aggregate(AggregateKind, Vec<Operand>),
    Cast(Operand, Ty),
}

#[term]
pub enum BinaryOp {
    #[cast]
    Math(BinaryMathOp),
    #[cast]
    Comparison(BinaryComparisonOp),
}

#[term]
pub enum BinaryMathOp {
    #[grammar(+)]
    Add,
    #[grammar(-)]
    Subtract,
    #[grammar(*)]
    Multiply,
    #[grammar(/)]
    Divide,
}

#[term]
pub enum BinaryComparisonOp {
    #[grammar(<)]
    LessThan,
    #[grammar(<=)]
    LessEqual,
    #[grammar(>)]
    GreaterThan,
    #[grammar(>=)]
    GreaterEqual,
}

#[term]
pub enum AggregateKind {
    Tuple,
    Adt(AdtId, VariantId, Vec<Parameter>),
}

#[term]
pub enum Terminator {
    Goto(BasicBlockId),
    Resume,
    Abort,
    Return,
    Unreachable,
    Drop(Place, Vec<BasicBlockId>),
    DropAndReplace(Place, Vec<BasicBlockId>),
    Call(Operand, Vec<Operand>, Place, Vec<BasicBlockId>),
}

#[term]
pub enum Operand {
    Move(Place),
    Copy(Place),
    Const(Constant),
}

#[term]
pub enum Constant {
    Number(usize),
    True,
    False,
    FnPtr(FnId, Vec<Parameter>),
    Tuple(Vec<Constant>),
}

#[term(($local_id $*projections))]
pub struct Place {
    pub local_id: LocalId,
    pub projections: Vec<Projection>,
}

#[term]
pub enum Projection {
    #[grammar(*)]
    Deref,

    #[grammar($v0)]
    Field(FieldId),

    #[grammar([$v0])]
    Index(LocalId),

    #[grammar((as $v0))]
    Downcast(VariantId),
}

#[term]
pub enum PlaceTy {
    #[cast]
    Ty(Ty),

    VariantTy(Ty, VariantId),
}

impl DowncastFrom<Parameter> for PlaceTy {
    fn downcast_from(t: &Parameter) -> Option<Self> {
        let t: Ty = t.downcast()?;
        Some(t.upcast())
    }
}
