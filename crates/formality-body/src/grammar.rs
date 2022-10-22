use formality_decl::grammar::VariantId;
use formality_macros::term;
use formality_types::grammar::{FieldId, FnId, Lt, Parameter, RefKind, Ty};

#[term]
pub enum FnBody {
    TrustedFnBody,
    #[cast]
    MirFnBody(MirFnBody),
}

#[term]
pub struct MirFnBody {}

#[term]
pub struct LocalsAndBlocks {
    local_decls: Vec<LocalDecl>,
    basic_block_decls: Vec<BasicBlockDecl>,
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
    id: BasicBlockId,
    statements: Vec<Statement>,
    terminator: Terminator,
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

#[term(($local $*projections))]
pub struct Place {
    local: LocalId,
    projections: Vec<Projection>,
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
